defmodule PersistentGenServer do
  use GenServer
  @moduledoc """
  PersistentGenServer makes your GenServers Persistent!


  # TODO possible dimensions for configurability:

  - [ ] Swap out process registries that PersistentGenServer.Registry wraps.
  - [ ] Let users choose between: (temporary/transient/permanent)
    - Wipe persistency for GenServer when it stops normally or crashes.
    - Wipe persistency for GenServer only when it stops normally.
    - Even restart GenServer from persistency when it crashed before.
  - [x] Other storage adapters.
  - [ ] Timeout length before a process petrifies itself.
  - [ ] Only write to cache on `terminate` vs during each handle_* for efficienty vs fault-tolerancy?
  - [ ] A mapping function between the actual state and the state-to-be-persisted/reloaded, to hide ephemeral parts.

  # Things to figure out:

  - How to re-create original link/monitor structure? Is there a way for a process to keep track of its supervision?


  """

  defmodule Config do
    require Specify
    Specify.defconfig do
      @doc """
      The actual storage implementation that will be used
      to persist the state of the running GenServer,
      so that it can be revived when it stopped.

      Should be a module name implementing the `PersistentGenServer.Storage` behaviour.
      """
      field :storage_implementation, :atom, default: PersistentGenServer.Storage.ETS

      @doc """
      A timeout before the GenServer auto-unloads ('petrifies') itself.

      '0' means: indefinite.
      TODO maybe change to`:infinity`?
      """
      field :petrification_timeout, :integer, default: 1

      @doc """
      The atom name (or module name) of the Dynamic Supervisor
      that should supervise this GenServer.
      """
      field :dynamic_supervisor, :atom, default: PersistentGenServer.GlobalSupervisor

      @doc """
      The process registry that is used to keep track of which
      processes are currently in a non-petrified state.

      By default the Elixir `Registry`-module is used,
      which works well, except in Special Circumstances.
      (for instance: on a distributed system this might need more thought).
      """
      field :subregistry, :atom, default: Registry

      @doc """
      These options are used whenever the server is (re)started.
      See `GenServer.start_link/3`'s "Options" section for the allowed values.
      """
      field :gen_server_options, {:list, :term}, default: []
    end
  end

  defstruct [:module, :init_args, :internal_state, config: Config.load(), petrification_timer_ref: nil]

  defp via_pid(module, init_args, config) do
    {:via, __MODULE__.Registry, {module, init_args, config}}
  end

  @doc """
  Starts the GenServer `module` with `init_args`.

  This function returns `{:ok, pid}` where `pid` is actually
  a `{:via, PersistentGenServer.Registry, ...}` tuple, which you
  can store and re-use to call the persistent GenServer later, even if stopped running in the meantime.


  In case you are wondering: There is no `start/3` alternative because it does not make sense
  to start a persistent GenServer without supervision.
  """
  def start_link(module, init_args, gen_server_options \\ []) do
    # TODO raise for unsupported GenServer options like `:name`.
    config = parse_config(gen_server_options)

    # NOTE: WE piggyback the call to the module's init in the call to our init, in the call to `GenServer.start_llink` in the child spec expected for DynamicSupervisor.start_child
    with {:ok, res} <- DynamicSupervisor.start_child(config.dynamic_supervisor, %{id: __MODULE__, start: {GenServer, :start_link, [__MODULE__, {module, init_args, config}, gen_server_options]}}) do
      IO.inspect(res, label: "DynamicSupervisor result")
      {:ok, via_pid(module, init_args, config)}
    end
  end

  defp parse_config(gen_server_options) do
    {passed_in_config, gen_server_options} = pop_in(gen_server_options[:persistent_gen_server_options])
    (passed_in_config || [])
    |> put_in([:gen_server_options], gen_server_options)
    |> Config.load_explicit
  end

  @impl true
  def init({module, init_args, config}) do
    case PersistentGenServer.Storage.ETS.read({module, init_args}) do
      {:ok, state} ->
        PersistentGenServer.Registry.register_name({module, init_args, state.config}, self())
        {:ok, state}
      :not_found ->
        with {:ok, internal_state} <- module.init(init_args),
             state = %__MODULE__{module: module, init_args: init_args, internal_state: internal_state, config: config},
             :ok <- persist!(state),
             :yes = PersistentGenServer.Registry.register_name({module, init_args, config}, self()),
             state = reset_petrification_timeout(state) do
          {:ok, state}
        end
      {:error, reason} ->
        {:error, reason}
    end
  end

  @impl true
  def handle_call(call, from, state = %__MODULE__{module: module, internal_state: internal_state}) do
    IO.inspect({call, from, state}, label: "handle_call")
    case module.handle_call(call, from, internal_state) do
      {:reply, reply, new_state} ->
        {:reply, reply, update_and_persist(state, new_state)}
      {:reply, reply, new_state, extra} ->
        {:reply, reply, update_and_persist(state, new_state), extra}

      {:noreply, reply, new_state} ->
        {:noreply, reply, update_and_persist(state, new_state)}
      {:noreply, reply, new_state, extra} ->
        {:noreply, reply, update_and_persist(state, new_state), extra}

      {:stop, reason, reply, new_state} ->
        {:stop, reason, reply, update_and_persist(state, new_state)}
      {:stop, reason, new_state} ->
        {:stop, reason, update_and_persist(state, new_state)}
      other ->
        other
    end
  end

  @impl true
  def handle_cast(call, state = %__MODULE__{module: module, internal_state: internal_state}) do
    case module.handle_cast(call, internal_state) do
      {:noreply, new_state} ->
        {:noreply, update_and_persist(state, new_state)}
      {:noreply, new_state, extra} ->
        {:noreply, update_and_persist(state, new_state), extra}

      {:stop, reason, new_state} ->
        {:stop, reason, update_and_persist(state, new_state)}
      other ->
        other
    end
  end

  @impl true
  def handle_continue(continue, state = %__MODULE__{module: module, internal_state: internal_state}) do
    case module.handle_continue(continue, internal_state) do
      {:noreply, new_state} ->
        {:noreply, update_and_persist(state, new_state)}
      {:noreply, new_state, extra} ->
        {:noreply, update_and_persist(state, new_state), extra}

      {:stop, reason, new_state} ->
        {:stop, reason, update_and_persist(state, new_state)}
      other ->
        other
    end
  end

  @impl true
  def handle_info(:"PersistentGenServer.petrification", state = %__MODULE__{module: module, internal_state: internal_state}) do
    IO.puts("Petrifying!")
    {:stop, {:shutdown, :"PersistentGenServer.petrification"}, state}
  end
  def handle_info(msg, state = %__MODULE__{module: module, internal_state: internal_state}) do
    case module.handle_info(msg, internal_state) do
      {:noreply, new_state} ->
        {:noreply, update_and_persist(state, new_state)}
      {:noreply, new_state, extra} ->
        {:noreply, update_and_persist(state, new_state), extra}

      {:stop, reason, new_state} ->
        {:stop, reason, update_and_persist(state, new_state)}
      other ->
        other
    end
  end

  defp persist!(state = %__MODULE__{}) do
    IO.inspect state, label: "persisting state"
    state = put_in(state.petrification_timer_ref, nil) # We do not want to persist this field ever, since it contains a Timer reference.

    # TODO nicer error handling?
    :ok = state.config.storage_implementation.store({state.module, state.init_args}, state)
    :ok
  end

  defp update_and_persist(old_state, new_internal_state) do
    new_state = put_in old_state.internal_state, new_internal_state
    persist!(new_state)

    reset_petrification_timeout(new_state)
  end

  defp reset_petrification_timeout(state) do
    if state.config.petrification_timeout == 0 do
      state
    else
      case state.petrification_timer_ref do
        ref when is_reference(ref) -> Process.cancel_timer(ref)
        nil -> :ok
      end
      new_timer_ref = Process.send_after(self(), :"PersistentGenServer.petrification", state.config.petrification_timeout)
      put_in(state.petrification_timer_ref, new_timer_ref)
    end
  end

  @impl true
  def terminate(reason, state) do
    IO.inspect({reason, state}, label: "Terminate called!")
    case reason do
      {:shutdown, :"PersistentGenServer.petrification"} ->
        :ok
      :normal ->
        state.module.terminate(reason, state.internal_state)
        wipe!(state)
        :ok
      other ->
        state.module.terminate(reason, state.internal_state)
        :ok
    end
  end

  defp wipe!(state = %__MODULE__{}) do
    # TODO nicer error handling?
    :ok = state.config.storage_implementation.wipe({state.module, state.init_args})
  end
end
