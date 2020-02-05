defmodule InductiveGraph.Utilities do
  @moduledoc """
  Functions for use by other modules.
  """

  @doc """
  Updates value at `key` in `map` by `function`.

  If `key` is present in `map` with value `value`, then `{:ok, new_map}` is
  returned where `new_map` is equivalent `map` with the result of `function`
  with argument `value` as the new value for `key`. Otherwise, `:error` is
  returned.
  """
  @spec map_update(map, Map.key, (Map.value -> Map.value)) :: {:ok, map} | :error
  def map_update(map, key, function) do
    with {:ok, value} <- Map.fetch(map, key) do
      value = function.(value)
      {:ok, Map.put(map, key, value)}
    else
      _error -> :error
    end
  end

  @doc """
  Updates element at `position` in `tuple` by `function`.
  """
  @spec tuple_update_position!(tuple, non_neg_integer, (term -> term)) :: tuple
  def tuple_update_position!(tuple, position, function) do
    element = elem(tuple, position)
    element = function.(element)
    put_elem(tuple, position, element)
  end
end
