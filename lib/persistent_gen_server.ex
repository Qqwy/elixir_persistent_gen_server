defmodule PersistentGenServer do
  use GenServer
  @moduledoc """
  PersistentGenServer makes your GenServers Persistent!


  # TODO possible dimensions for configurability:

  - Swap out process registries that PersistentGenServer.Registry wraps.
  - Optionally Auto-start GenServers when called for the first time with a `:via`-tuple when not persisted yet. (Current behaviour is to require at least once a manual start using `start`/ `start_link`)
  - Let users choose between: (temporary/transient/permanent)
    - Wipe persistency for GenServer when it stops normally or crashes.
    - Wipe persistency for GenServer only when it stops normally.
    - Even restart GenServer from persistency when it crashed before.
  - Other storage adapters.
  - Timeout length before a process petrifies itself.
  - Only write to cache on `terminate` vs during each handle_* for efficienty vs fault-tolerancy?
  - A mapping function between the actual state and the state-to-be-persisted/reloaded, to hide ephemeral parts.


  # Things to figure out:

  - How to re-create original link/monitor structure? Is there a way for a process to keep track of its supervision?


  """

  defmodule Config do
    require Specify
    Specify.defconfig do
      field :storage_implementation, :atom, default: PersistentGenServer.Storage.ETS
      field :petrification_timeout, :integer, default: 0
      field :dynamic_supervisor, :atom, default: PersistentGenServer.GlobalSupervisor
      field :gen_server_options, {:list, :term}, default: []
    end
  end

  defstruct [:module, :init_args, :internal_state, config: Config.load()]


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
             :ok <- persist!(state) do
          PersistentGenServer.Registry.register_name({module, init_args, config}, self())
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

    # TODO nicer error handling?
    :ok = state.config.storage_implementation.store({state.module, state.init_args}, state)
  end

  defp update_and_persist(old_state, new_internal_state) do
    new_state = put_in old_state.internal_state, new_internal_state
    persist!(new_state)

    new_state
  end

  @impl true
  def terminate(reason, state) do
    IO.inspect({reason, state}, label: "Terminate called!")
    case reason do
      :normal ->
        wipe!(state)
        :ok
      _ ->
        :ok
    end
  end

  defp wipe!(state = %__MODULE__{}) do
    # TODO nicer error handling?
    :ok = state.config.storage_implementation.wipe({state.module, state.init_args})
  end
end
