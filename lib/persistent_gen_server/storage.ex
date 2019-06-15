defmodule PersistentGenServer.Storage do
  @callback store(tuple(), any()) :: :ok | {:error, reason :: any()}

  @callback read(tuple()) :: {:ok, any()} | :not_found | {:error, reason :: any()}

  @callback wipe(tuple()) :: :ok | {:error, reason :: any()}
end

defmodule PersistentGenServer.Storage.ETS do
  @behaviour PersistentGenServer.Storage

  @impl true
  def store(identity_tuple, value) do
    ensure_table_exists!()

    IO.inspect({identity_tuple, value}, label: "storing")

    :ets.insert(__MODULE__, {identity_tuple, value})
    :ok
  end

  @impl true
  def read(identity_tuple) do
    ensure_table_exists!()
    IO.inspect(:ets.tab2list(__MODULE__), label: "reading")
    case :ets.lookup(__MODULE__, identity_tuple) do
      [] -> :not_found
      [{_, state}] -> {:ok, state}
      other_result -> {:error, "invalid item stored in table of PersistentGenServer.Storage.ETS:", other_result}
    end
  end

  @impl true
  def wipe(identity_tuple) do
    ensure_table_exists!()
    true = :ets.delete(__MODULE__, identity_tuple)
    :ok
  end

  # Used for test cleanup,
  # but not very useful in general
  # and thus not part of the public API
  @doc false
  def clear_all do
    if :ets.whereis(__MODULE__) != :undefined do
      :ets.delete(__MODULE__)
    end
  end

  defp ensure_table_exists! do
    if :ets.whereis(__MODULE__) == :undefined  do
      :ets.new(__MODULE__, [:set, :public, :named_table])
    end
  end
end
