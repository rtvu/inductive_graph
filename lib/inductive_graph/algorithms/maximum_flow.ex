defmodule InductiveGraph.Algorithms.MaximumFlow do
  @moduledoc """
  Functions to calculate maximum flow using the Edmonds-Karp algorithm.
  """

  def list_reversed_tagged_edges(tagged_edges) do
    reverse_edges =
      fn
        {from_vertex, to_vertex, _edge_value}, accumulator ->
          [{to_vertex, from_vertex, 0} | accumulator]
      end

    tagged_edges
    |> Enum.reduce([], reverse_edges)
  end


end
