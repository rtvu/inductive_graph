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

    {:ok, graph} = Graph.insert_tagged_edges(graph, reversed_tagged_edges)
    Graph.map_edges(graph, fn i -> {i, 0, i} end)
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

  def get_maximum_flow(graph, source, sink) do
    graph =
      graph
      |> format_graph()
      |> get_maximum_flow_helper(source, sink)
      |> Graph.filter_edges(fn {maximum_capacity, _current_flow, _residual_capcity} -> maximum_capacity != 0 end)
      |> Graph.map_edges(fn {maximum_capacity, current_flow, _residual_capcity} -> {current_flow, maximum_capacity} end)

    {:ok, {_predecessors, _vertex, _vertex_value, successors}, _graph} = Graph.decompose(graph, source)
    flow = Enum.reduce(successors, 0, fn {{current_flow, _maximum_capacity}, _neighbor}, flow -> current_flow + flow end)
    {flow, graph}
  end

  defp get_maximum_flow_helper(graph, source, sink) do
    nonzero_graph = Graph.filter_edges(graph, fn {_maximum_capacity, _current_flow, residual_capacity} -> residual_capacity != 0 end)
    case find_tagged_path(nonzero_graph, source, sink) do
      :error ->
        graph

      {:ok, tagged_path} ->
        {path, flow} = List.foldr(tagged_path, {[], nil}, fn {vertex, value}, {path, mininum_value} -> {[vertex | path], mininum(value, mininum_value)} end)
        graph = update_flow(graph, path, flow)
        get_maximum_flow_helper(graph, source, sink)
    end
  end

  defp mininum(left, right) do
    case {left, right} do
      {nil, right} -> right
      {left, nil} -> left
      {left, right} -> min(left, right)
    end
  end


  def find_tagged_path(graph, source, sink) do
    path = [{source, nil}]
    queue = :queue.from_list([path])
    find_tagged_path_helper(graph, sink, queue)
  end

  defp find_tagged_path_helper(graph, sink, queue) do
    if :queue.is_empty(queue) or Graph.empty?(graph) do
      :error
    else
      {{:value, path}, queue} = :queue.out(queue)
      [{next_vertex, _next_edge_value}  | _rest_of_path] = path
      if next_vertex == sink do
        {:ok, Enum.reverse(path)}
      else
        case Graph.decompose(graph, next_vertex) do
          {:ok, context, graph} ->
            {_predecessors, _vertex, _vertex_value, successors} = context
            new_paths = Enum.map(successors, fn {{_maximum_capacity, _current_flow, residual_capacity}, neighbor} -> [{neighbor, residual_capacity} | path] end)
            queue = :queue.join(queue, :queue.from_list(new_paths))
            find_tagged_path_helper(graph, sink, queue)
          :error ->
            find_tagged_path_helper(graph, sink, queue)
        end
      end
    end
  end




end
