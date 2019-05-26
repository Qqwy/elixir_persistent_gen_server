defmodule PersistentGenServer.Application do
  use Application
  @impl true
  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      {Registry, keys: :unique, name: PersistentGenServer.Registry}
    ]
    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
