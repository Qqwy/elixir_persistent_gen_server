defmodule PersistentGenServer.Registry do
  @moduledoc """
  Custom registry that attempts to load process from persistency if it is not started but existed before.

  Currently it builds on top of Elixir's `Registry`, although there is no reason that it couldn't wrap other registries as well in the future.
  """

  @doc false
  def whereis_name({module_name, init_args}) do
    IO.inspect({module_name, init_args}, label: "whereis_name called!")
    case Registry.whereis_name({__MODULE__, {module_name, init_args}}) do
      pid when is_pid(pid) ->
        pid
      :undefined ->
        attempt_revive({module_name, init_args})
    end
  end

  defp attempt_revive({module_name, init_args}) do
    # case PersistentGenServer.Storage.ETS.read({module_name, init_args}) do
    #   {:ok, val} ->
    #     IO.inspect({"Loading GenServer from persistency", module_name, init_args, val})
        {:ok, pid} =
          DynamicSupervisor.start_child(
            PersistentGenServer.GlobalSupervisor,
            %{id: PersistentGenserver,
              start: {GenServer,
                      :start_link,
                      [PersistentGenServer, {module_name, init_args}, ]}}
          ) # , [name: {:via, PersistentGenServer.Registry, {module_name, init_args}}])
        IO.inspect({"NEW PID:", pid})
        pid
      #   pid
      # :not_found ->
      #   IO.inspect({"Attempting to load module from persistency, but was not found", module_name, init_args})
      #   :undefined
      # {:error, reason} ->
      #   IO.inspect({"Error: ", reason})
      #   raise reason
    # end
  end

  @doc false
  def register_name({module, init_args}, pid) do
    IO.puts "REGISTER NAME called!"
    IO.inspect({{module, init_args}, pid}, label: :register_name)
    Registry.register_name({__MODULE__, {module, init_args}}, pid)
  end

  @doc false
  def send({module, key}, msg) do
    IO.puts "SEND called!"
    IO.inspect({{module, key}, msg}, label: :send)
    case whereis_name({module, key}) do
      pid when is_pid(pid) ->
        IO.inspect(pid, label: "SEND called and process potentially revived!")
        Kernel.send(pid, msg)
      other -> other
    end
    # Registry.send({__MODULE__, {module, key}}, msg)
  end

  @doc false
  def unregister_name({module, key}) do
    IO.puts "UNREGISTER NAME called!"
    IO.inspect({module, key}, label: :unregister_name)
    Registry.unregister_name({__MODULE__, {module, key}})
  end
end
