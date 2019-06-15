ExUnit.start()

defmodule Example do
  defstruct [name: "", score: 42]
  use GenServer

  def start_ephemeral(name) do
    GenServer.start(__MODULE__, name)
  end

  def start_persistent(name) do
    PersistentGenServer.start(__MODULE__, name)
  end

  def increment_score(pid) do
    GenServer.cast(pid, :increment_score)
  end

  def read_score(pid) do
    GenServer.call(pid, :read_score)
  end

  @impl true
  def init(user_name) do
    {:ok, %__MODULE__{name: user_name, score: 0}}
  end

  @impl true
  def handle_cast(:increment_score, state) do
    {:noreply, update_in(state.score, &(&1 + 1))}
  end

  @impl true
  def handle_call(:read_score, from, state) do
    {:reply, state.score, state}
  end
end

