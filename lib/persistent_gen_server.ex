defmodule PersistentGenServer do
  @moduledoc """
  PersistentGenServer makes your GenServers Persistent!
  """

  defstruct [:module, :init_args, :internal_state, storage_impl: PersistentGenServer.Storage.ETS]


  @doc """
  Starts the GenServer `module` with `init_args`.

  This function returns `{:ok, pid}` where `pid` is actually
  a `{:via, PersistentGenServer.Registry, ...}` tuple, which you
  can store and re-use to call the persistent GenServer later, even if stopped running in the meantime.
  """
  def start_link(module, init_args, gen_server_options \\ []) do
    # TODO extract/use persistency options
    # TODO don't start if GenServer already is started?
    # TODO load from persistency if persisted before
    gen_server_options = put_in(gen_server_options[:name], {:via, PersistentGenServer.Registry, {module, init_args}})
    GenServer.start_link(__MODULE__, {module, init_args}, gen_server_options)

    {:ok, gen_server_options[:name]}
  end

  def start(module, init_args, gen_server_options \\ []) do
    # TODO extract/use persistency options
    # TODO don't start if GenServer already is started?
    # TODO load from persistency if persisted before
    GenServer.start(__MODULE__, {module, init_args}, gen_server_options)

    {:ok, gen_server_options[:name]}
  end

  def init({module, init_args}) do
    with {:ok, internal_state} <- module.init(init_args),
         state = %__MODULE__{module: module, init_args: init_args, internal_state: internal_state},
           :ok <- persist!(state) do
           {:ok, state}
    end
  end

  def handle_call(call, from, state = %__MODULE__{module: module, internal_state: internal_state}) do
    # put_in state.internal_state, module.handle_call(call, internal_state)
    # # TODO persist
    # |> persist()
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
        # TODO: Remove state from persistency?
        {:stop, reason, reply, update_and_persist(state, new_state)}
      {:stop, reason, new_state} ->
        # TODO: Remove state from persistency?
        {:stop, reason, update_and_persist(state, new_state)}
      other ->
        other
    end
  end

  def handle_cast(call, state = %__MODULE__{module: module, internal_state: internal_state}) do
    IO.puts({call, state}, label: "A")
    case module.handle_cast(call, internal_state) do
      {:noreply, new_state} ->
        {:noreply, update_and_persist(state, new_state)}
      {:noreply, new_state, extra} ->
        {:noreply, update_and_persist(state, new_state), extra}

      {:stop, reason, new_state} ->
        # TODO: Remove state from persistency?
        {:stop, reason, update_and_persist(state, new_state)}
      other ->
        other
    end
  end

  def handle_continue(continue, state = %__MODULE__{module: module, internal_state: internal_state}) do
    case module.handle_continue(continue, internal_state) do
      {:noreply, new_state} ->
        {:noreply, update_and_persist(state, new_state)}
      {:noreply, new_state, extra} ->
        {:noreply, update_and_persist(state, new_state), extra}

      {:stop, reason, new_state} ->
        # TODO: Remove state from persistency?
        {:stop, reason, update_and_persist(state, new_state)}
      other ->
        other
    end
  end

  def handle_info(msg, state = %__MODULE__{module: module, internal_state: internal_state}) do
    case module.handle_info(msg, internal_state) do
      {:noreply, new_state} ->
        {:noreply, update_and_persist(state, new_state)}
      {:noreply, new_state, extra} ->
        {:noreply, update_and_persist(state, new_state), extra}

      {:stop, reason, new_state} ->
        # TODO: Remove state from persistency?
        {:stop, reason, update_and_persist(state, new_state)}
      other ->
        other
    end
  end

  defp persist!(state) do
    IO.inspect state, label: "persisting state"

    # TODO nicer error handling
    :ok = state.storage_impl.store({state.module, state.init_args}, state)
  end

  defp update_and_persist(old_state, new_internal_state) do
    new_state = put_in old_state.internal_state, new_internal_state
    persist!(new_state)

    new_state
  end
end
