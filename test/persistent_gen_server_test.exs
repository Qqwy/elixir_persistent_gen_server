defmodule PersistentGenServerTest do
  use ExUnit.Case
  doctest PersistentGenServer

  test "greets the world" do
    assert PersistentGenServer.hello() == :world
  end
end
