defmodule PersistentGenServerTest do
  use ExUnit.Case
  doctest PersistentGenServer

  defmodule Example do
    defstruct [name: "", score: 42]
    use GenServer

    @impl true
    def init(user_name) do
      {:ok, %__MODULE__{name: user_name, score: 0}}
    end

    def handle_cast(:increment_score, state) do
      {:noreply, update_in(state.score, &(&1 + 1))}
    end

    def handle_cast(:read_score, state) do
      {:reply, state.score, state}
    end
  end

  
end
