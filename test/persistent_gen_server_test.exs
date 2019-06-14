defmodule PersistentGenServerTest do
  use ExUnit.Case
  doctest PersistentGenServer

  use ExUnit.Case, async: true

  describe "the example GenServer works as expected without being wrapped by PersistentGenServer" do
    test "Basic operation" do
      {:ok, pid} = Example.start_link_ephemeral("Vorpal")

      assert 0 == Example.read_score(pid)
      Example.increment_score(pid)
      assert 1 == Example.read_score(pid)

      Example.increment_score(pid)
      Example.increment_score(pid)
      Example.increment_score(pid)
      assert 4 == Example.read_score(pid)

      assert :ok == GenServer.stop(pid, :normal)
      assert {:noproc, _} = catch_exit(Example.read_score(pid))
    end
  end
end
