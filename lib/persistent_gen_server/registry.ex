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
        IO.inspect("I would like to load it here")
        :undefined
    end
  end

  @doc false
  def register_name({module, init_args}, pid) do
    Registry.register_name({__MODULE__, {module, init_args}}, pid)
  end
end
