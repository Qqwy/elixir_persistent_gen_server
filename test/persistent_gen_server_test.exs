defmodule PersistentGenServerTest do
  use ExUnit.Case
  doctest PersistentGenServer

  use ExUnit.Case, async: true

  setup do
    on_exit fn ->
      PersistentGenServer.Storage.ETS.clear_all
    end
  end

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

  describe "Wrapping the Example GenServer with the PersistentGenServer" do
    test "Basic behaviour is unaltered" do
      {:ok, pid} = Example.start_link_persistent("Vorpal")

      assert 0 == Example.read_score(pid)
      Example.increment_score(pid)
      assert 1 == Example.read_score(pid)

      Example.increment_score(pid)
      Example.increment_score(pid)
      Example.increment_score(pid)
      assert 4 == Example.read_score(pid)

      assert :ok == GenServer.stop(pid, :normal)
      assert {:shutdown, _} = catch_exit(Example.read_score(pid))
    end

    test "Server will restart if not stopped with ':normal' reason " do
      {:ok, pid} = Example.start_link_persistent("Vorpal")

      assert 0 == Example.read_score(pid)
      Example.increment_score(pid)
      assert 1 == Example.read_score(pid)

      Example.increment_score(pid)
      Example.increment_score(pid)
      Example.increment_score(pid)
      assert 4 == Example.read_score(pid)

      assert :ok == GenServer.stop(pid, :asdf)

      assert 4 == Example.read_score(pid)

      # assert :ok == GenServer.stop(pid, :normal)
    end
  end
end
