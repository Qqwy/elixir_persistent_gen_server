defmodule PersistentGenServer.Application do
  use Application
  @impl true
  def start(_type, _args) do
    children = [
      {Registry, keys: :unique, name: PersistentGenServer.Registry},
      {DynamicSupervisor,
       name: PersistentGenServer.GlobalSupervisor, strategy: :one_for_one, restart: :transient}
    ]

    :ets.new(PersistentGenServer.Storage.ETS, [:set, :public, :named_table])

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
