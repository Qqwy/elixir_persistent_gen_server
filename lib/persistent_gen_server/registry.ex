defmodule PersistentGenServer.Registry do
  @moduledoc """
  Custom registry that attempts to load process from persistency if it is not started but existed before.
  """

  # Via callback
  @doc false
  def whereis_name({module_name, init_args}) do
    case Registry.whereis_name({__MODULE__, {module_name, init_args}}) do
      pid when is_pid(pid) -> pid
      :undefined ->
        # TODO try loading from persistency
        # TODO fix infinite loading loop.
        IO.inspect("I would like to load it here")
        case PersistentGenServer.Storage.ETS.read({module_name, init_args}) do
          {:ok, val} ->
            {:ok, pid} = GenServer.start(PersistentGenServer, {module_name, init_args}, [name: {:via, PersistentGenServer.Registry, {module_name, init_args}}])
            pid
          :not_found -> :undefined
          {:error, reason} -> raise reason
        end
    end
  end

  @doc false
  def register_name({module, init_args}, pid) do
    Registry.register_name({__MODULE__, {module, init_args}}, pid)
  end

  @doc false
  def send({module, key}, msg) do
    Registry.send({__MODULE__, {module, key}}, msg)
  end

  @doc false
  def unregister_name({module, key}) do
    Registry.unregister_name({__MODULE__, {module, key}})
  end

end