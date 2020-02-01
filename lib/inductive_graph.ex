defmodule InductiveGraph do
  @moduledoc """
  Documentation for InductiveGraph.
  """

  alias InductiveGraph, as: Graph

  @typedoc """
  Inductive graph
  """
  @opaque t :: %__MODULE__{}
  @typedoc """
  Label
  """
  @type label :: term
  @typedoc """
  Vertex
  """
  @type vertex :: integer
  @typedoc """
  Adjacents
  """
  @type adj :: [{label, vertex}]
  @typedoc """
  Predecessors
  """
  @type pred :: adj
  @typedoc """
  Successors
  """
  @type succ :: adj
  @typedoc """
  Context
  """
  @type context :: {pred, vertex, label, succ}

  # Internal adjacents
  @typep iadj :: %{required(vertex) => [label]}
  # Internal predecessors
  @typep ipreds :: iadj
  # Internal successors
  @typep isuccs :: iadj
  # Internal context
  @typep icontext :: {ipreds, label, isuccs}
  # Internal graph
  @typep igraph :: %{required(vertex) => icontext}

  defstruct [
    internal: %{},
  ]

  defimpl Inspect do
    def inspect(%Graph{}, _opts) do
      "#InductiveGraph<>"
    end
  end

  @doc """
  Creates an empty inductive graph.

  ## Examples

      iex> InductiveGraph.new()
      #InductiveGraph<>

  """
  @spec new() :: t
  def new(), do: %Graph{}

  @doc """
  Determines if the inductive graph is empty.

  ## Examples

      iex> graph = InductiveGraph.new()
      iex> InductiveGraph.empty?(graph)
      true
      iex> {:ok, graph} = InductiveGraph.make_graph([{1, 1}], [])
      iex> InductiveGraph.empty?(graph)
      false

  """
  @spec empty?(t) :: boolean
  def empty?(graph)
  def empty?(%Graph{internal: map = %{}}) do
    map == %{}
  end

  @doc """
  Builds a graph from a list of vertices and edges.

  ## Examples

      iex> vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(vertices, edges)
      iex> {:ok, context1, graph} = InductiveGraph.decompose_by_vertex(graph, 3)
      iex> context1
      {[{"down", 2}], 3, "c", [{"up", 1}]}
      iex> {:ok, context2, graph} = InductiveGraph.decompose_by_vertex(graph, 2)
      iex> context2
      {[{"right", 1}], 2, "b", [{"left", 1}]}
      iex> {:ok, context3, graph} = InductiveGraph.decompose_by_vertex(graph, 1)
      iex> context3
      {[], 1, "a", []}
      iex> InductiveGraph.empty?(graph)
      true

  """
  @spec make_graph([{vertex, label}], [{vertex, vertex, label}]) :: {:ok, t} | :error
  def make_graph(vertices, edges) do
    transform_vertex = fn {vertex, label} -> {vertex, {%{}, label, %{}}} end
    wrap_map = fn map -> %Graph{internal: map} end

    vertices
    |> Stream.map(transform_vertex)
    |> Map.new()
    |> wrap_map.()
    |> insert_edges(edges)
  end

  @doc """
  Inserts edges into graph.

  TODO: Examples need other functions to be defined first.

  ## Examples

      iex> InductiveGraph.new()
      #InductiveGraph<>

  """
  @spec insert_edges(t, [{vertex, vertex, label}]) :: {:ok, t} | :error
  def insert_edges(graph, edges) do
    insert =
      fn
        edge, {:ok, graph} -> insert_edge(graph, edge)
        _edge, :error -> :error
      end

    List.foldl(edges, {:ok, graph}, insert)
  end

  @doc """
  Inserts an edge into graph.

  TODO: Examples need other functions to be defined first.

  ## Examples

      iex> InductiveGraph.new()
      #InductiveGraph<>

  """
  @spec insert_edge(t, {vertex, vertex, label}) :: {:ok, t} | :error
  def insert_edge(graph, edge)
  def insert_edge(%Graph{internal: map}, {source, target, label}) do
    with {:ok, source_context} <- Map.fetch(map, source),
         {:ok, target_context} <- Map.fetch(map, target),
         updated_source_context <- update_internal_context(source_context, {target, label}, :successors),
         updated_target_context <- update_internal_context(target_context, {source, label}, :predecessors) do
      {:ok, %Graph{internal: %{map | source => updated_source_context, target => updated_target_context}}}
    else
      _error -> :error
    end
  end

  # Updates an element within internal context.
  @spec update_internal_context(icontext, label, :predecessors | :label | :successors) :: icontext
  defp update_internal_context(context, value, location)
  defp update_internal_context({predecessors, _label, successors}, label, :label) do
    {predecessors, label, successors}
  end
  defp update_internal_context({predecessors, label, successors}, {target, target_label}, :successors) do
    successors = Map.update(successors, target, [target_label], &Enum.concat(&1, [target_label]))
    {predecessors, label, successors}
  end
  defp update_internal_context({predecessors, label, successors}, {source, source_label}, :predecessors) do
    predecessors = Map.update(predecessors, source, [source_label], &Enum.concat(&1, [source_label]))
    {predecessors, label, successors}
  end

  @doc """
  Decomposes the graph into a context containing vertex and the remaining graph.

  ## Examples

      iex> vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(vertices, edges)
      iex> {:ok, context1, graph} = InductiveGraph.decompose_by_vertex(graph, 3)
      iex> context1
      {[{"down", 2}], 3, "c", [{"up", 1}]}
      iex> {:ok, context2, graph} = InductiveGraph.decompose_by_vertex(graph, 2)
      iex> context2
      {[{"right", 1}], 2, "b", [{"left", 1}]}
      iex> {:ok, context3, graph} = InductiveGraph.decompose_by_vertex(graph, 1)
      iex> context3
      {[], 1, "a", []}
      iex> InductiveGraph.empty?(graph)
      true

  """
  @spec decompose_by_vertex(t, vertex) :: {:ok, context, t} | :error
  def decompose_by_vertex(graph, vertex)
  def decompose_by_vertex(%Graph{internal: map}, vertex) do
    with {:ok, {predecessors, label, successors}} <- Map.fetch(map, vertex) do
      vertex_removed_map = Map.delete(map, vertex)

      vertex_removed_predecessors = Map.delete(predecessors, vertex)
      vertex_removed_predecessors_keys = Map.keys(vertex_removed_predecessors)
      vertex_removed_map = remove_vertex_from_internal_adjacents(vertex_removed_map, vertex, vertex_removed_predecessors_keys, :successors)

      vertex_removed_successors = Map.delete(successors, vertex)
      vertex_removed_successors_keys = Map.keys(vertex_removed_successors)
      vertex_removed_map = remove_vertex_from_internal_adjacents(vertex_removed_map, vertex, vertex_removed_successors_keys, :predecessors)

      predecessors = convert_to_adjacents(vertex_removed_predecessors)
      successors = convert_to_adjacents(successors)

      {:ok, {predecessors, vertex, label, successors}, %Graph{internal: vertex_removed_map}}
    else
      _error -> :error
    end
  end

  # Removes vertex from internal context's predcessors or successors field for
  # every target vertex.
  @spec remove_vertex_from_internal_adjacents(igraph, vertex, [vertex], :predecessors | :successors) :: igraph
  defp remove_vertex_from_internal_adjacents(map, vertex, target_vertices, adjacents_type)
  defp remove_vertex_from_internal_adjacents(map, _vertex, [], _adjacents_type), do: map
  defp remove_vertex_from_internal_adjacents(map, vertex, [target_vertex | target_vertices], adjacents_type) do
    {predecessors, label, successors} = Map.fetch!(map, target_vertex)
    context =
      case adjacents_type do
        :predecessors -> {Map.delete(predecessors, vertex), label, successors}
        :successors -> {predecessors, label, Map.delete(successors, vertex)}
      end
    map = Map.put(map, target_vertex, context)
    remove_vertex_from_internal_adjacents(map, vertex, target_vertices, adjacents_type)
  end

  # Converts internal adjacents to adjacents.
  @spec convert_to_adjacents(iadj) :: adj
  defp convert_to_adjacents(map) do
    map
    |> Map.to_list()
    |> Enum.flat_map(fn {key, values} -> Enum.map(values, &({&1, key})) end)
  end

  @doc """
  Lists all vertices in graph.

  ## Examples

      iex> vertices = [{1, "1"}, {2, "2"}, {3, "3"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(vertices, [])
      iex> graph |> InductiveGraph.list_vertices() |> Enum.sort()
      [{1, "1"}, {2, "2"}, {3, "3"}]

  """
  @spec list_vertices(t) :: [{vertex, label}]
  def list_vertices(graph)
  def list_vertices(%Graph{internal: map}) do
    format = fn {vertex, {_predecessors, label, _successors}} -> {vertex, label} end

    map
    |> Map.to_list()
    |> Enum.map(format)
  end
end
