defmodule InductiveGraph.Algorithms.MaximumFlow do
  @moduledoc """
  Functions to calculate maximum flow using the Edmonds-Karp algorithm.
  """

  alias InductiveGraph, as: Graph

  @doc """
  Creates a list of reversed edges based on `tagged_edges` with 0 maximum
  capacity.
  """
  def list_reversed_tagged_edges(tagged_edges) do
    reverse_edges =
      fn
        {from_vertex, to_vertex, _edge_value}, accumulator ->
          [{to_vertex, from_vertex, 0} | accumulator]
      end

    Enum.reduce(tagged_edges, [], reverse_edges)
  end

  def format_graph(graph) do
    reversed_tagged_edges =
      graph
      |> Graph.list_tagged_edges()
      |> list_reversed_tagged_edges()

    case Graph.insert_tagged_edges(graph, reversed_tagged_edges) do
      {:ok, graph} -> {:ok, Graph.map_edges(graph, fn i -> {i, 0, i} end)}
      _error -> :error
    end
  end

  def update_adjacents_flow(adjacents, neighbor, flow) do
    update =
      fn
        {{maximum_capacity, current_flow, residual_capacity}, ^neighbor} -> {{maximum_capacity, current_flow + flow, residual_capacity - flow}, neighbor}
        no_change -> no_change
      end

    Enum.map(adjacents, update)
  end

  def update_flow(graph, path, flow)
  def update_flow(graph, [], _flow), do: graph
  def update_flow(graph, [_from_vertex], _flow), do: graph
  def update_flow(graph, [from_vertex, to_vertex | rest], flow) do
    {:ok, context, graph} = Graph.decompose(graph, from_vertex)
    {predecessors, vertex, vertex_value, successors} = context
    predecessors = update_adjacents_flow(predecessors, to_vertex, -flow)
    successors = update_adjacents_flow(successors, to_vertex, flow)
    context = {predecessors, vertex, vertex_value, successors}
    {:ok, graph} = Graph.merge(graph, context)
    update_flow(graph, [to_vertex | rest], flow)
  end
end