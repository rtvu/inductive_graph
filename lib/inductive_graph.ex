defmodule InductiveGraph do
  @moduledoc """
  `InductiveGraph` implements graph functionality using an inductive defintion
  for graphs.
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

  @doc ~S"""
  Pretty prints inductive representation of graph.

  ## Examples

      iex> vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(vertices, edges)
      iex> InductiveGraph.pretty_print(graph) <> "\n"
      ~s'''
      | {[{"down", 2}], 3, "c", [{"up", 1}]}
      & {[{"right", 1}], 2, "b", [{"left", 1}]}
      & {[], 1, "a", []}
      & Empty
      '''

  """
  @spec pretty_print(t, integer) :: String.t
  def pretty_print(graph, count \\ -1)
  def pretty_print(graph = %Graph{internal: map}, count) do
    vertices = map |> Map.keys() |> Enum.sort() |> Enum.reverse()
    pretty_print(graph, vertices, count, "| ")
  end

  # Pretty prints inductive representation of graph.
  @spec pretty_print(t, [vertex], integer, String.t) :: String.t
  defp pretty_print(graph, vertices, count, result)
  defp pretty_print(_graph, [], _count, result), do: result <> "Empty"
  defp pretty_print(_graph, _vertices, 0, result), do: result <> "InductiveGraph"
  defp pretty_print(graph, [vertex | vertices], count, result) do
    {:ok, context, graph} = decompose_by_vertex(graph, vertex)
    result = result <> inspect(context) <> "\n& "
    pretty_print(graph, vertices, count - 1, result)
  end

  @doc """
  Creates an empty graph.

  ## Examples

      iex> graph = InductiveGraph.new()
      #InductiveGraph<>
      iex> InductiveGraph.pretty_print(graph)
      "| Empty"

  """
  @spec new() :: t
  def new(), do: %Graph{}

  @doc """
  Determines if graph is empty.

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
      iex> InductiveGraph.list_vertices(graph) |> Enum.sort()
      [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> InductiveGraph.list_edges(graph) |> Enum.sort()
      [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]

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

  @doc ~S"""
  Inserts edges into graph.

  ## Examples

      iex> {:ok, graph} = InductiveGraph.make_graph([{1, 1}, {2, 2}, {3, 3}], [])
      iex> {:ok, graph} = InductiveGraph.insert_edges(graph, [{1, 2,"right"}, {3, 2, "left"}])
      iex> InductiveGraph.pretty_print(graph) <> "\n"
      ~s'''
      | {[], 3, 3, [{"left", 2}]}
      & {[{"right", 1}], 2, 2, []}
      & {[], 1, 1, []}
      & Empty
      '''

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

  @doc ~S"""
  Inserts an edge into graph.

  ## Examples

      iex> {:ok, graph} = InductiveGraph.make_graph([{1, 1}, {2, 2}, {3, 3}], [{1, 2, "right"}])
      iex> {:ok, graph} = InductiveGraph.insert_edge(graph, {3, 2, "left"})
      iex> InductiveGraph.pretty_print(graph) <> "\n"
      ~s'''
      | {[], 3, 3, [{"left", 2}]}
      & {[{"right", 1}], 2, 2, []}
      & {[], 1, 1, []}
      & Empty
      '''

  """
  @spec insert_edge(t, {vertex, vertex, label}) :: {:ok, t} | :error
  def insert_edge(graph, edge)
  def insert_edge(%Graph{internal: map}, {source, target, label}) do
    with {:ok, map} <- update_internal_map(map, source, {target, [label]}, :successors),
         {:ok, map} <- update_internal_map(map, target, {source, [label]}, :predecessors) do
      {:ok, %Graph{internal: map}}
    else
      _error -> :error
    end
  end

  # Updates a vertex's internal context.
  @spec update_internal_map(map, vertex, term, :predecessors | :label | :successors | :context) :: {:ok, map} | :error
  defp update_internal_map(map, vertex, value, location)
  defp update_internal_map(map, vertex, value, :context), do: {:ok, Map.put(map, vertex, value)}
  defp update_internal_map(map, vertex, value, location) do
    with {:ok, context} <- Map.fetch(map, vertex) do
      context = update_internal_context(context, value, location)
      map = Map.put(map, vertex, context)
      {:ok, map}
    else
      _error -> :error
    end
  end

  # Updates an element within internal context.
  @spec update_internal_context(icontext, term, :predecessors | :label | :successors) :: icontext
  defp update_internal_context(context, value, location)
  defp update_internal_context({predecessors, _label, successors}, label, :label) do
    {predecessors, label, successors}
  end
  defp update_internal_context({predecessors, label, successors}, {target, target_labels}, :successors) do
    successors = update_internal_adjacents(successors, target, target_labels)
    {predecessors, label, successors}
  end
  defp update_internal_context({predecessors, label, successors}, {source, source_labels}, :predecessors) do
    predecessors = update_internal_adjacents(predecessors, source, source_labels)
    {predecessors, label, successors}
  end

  # Updates internal adjacent with a new edge.
  @spec update_internal_adjacents(iadj, vertex, label) :: iadj
  defp update_internal_adjacents(internal_adjacents, vertex, labels) do
      Map.update(internal_adjacents, vertex, labels, &Enum.concat(&1, labels))
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

  # Removes vertex from internal context's predecessors or successors field for
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

      iex> vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(vertices, [])
      iex> graph |> InductiveGraph.list_vertices() |> Enum.sort()
      [{1, "a"}, {2, "b"}, {3, "c"}]

  """
  @spec list_vertices(t) :: [{vertex, label}]
  def list_vertices(graph)
  def list_vertices(%Graph{internal: map}) do
    format = fn {vertex, {_predecessors, label, _successors}} -> {vertex, label} end

    map
    |> Map.to_list()
    |> Enum.map(format)
  end

  @doc """
  Counts the number of vertices in graph.

  ## Examples

      iex> vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(vertices, [])
      iex> InductiveGraph.count_vertices(graph)
      3

  """
  @spec count_vertices(t) :: non_neg_integer
  def count_vertices(%Graph{internal: map}) do
    map_size(map)
  end

  @doc """
  Gets range of vertices.

  ## Examples

      iex> vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(vertices, [])
      iex> InductiveGraph.vertex_range(graph)
      {:ok, 1, 3}

  """
  @spec vertex_range(t) :: {:ok, min :: integer, max :: integer} | :error
  def vertex_range(%Graph{internal: map}) do
    case Map.keys(map) do
      [] ->
        :error
      [vertex | vertices] ->
        min_max = fn vertex, {minimum, maximum} -> {min(minimum, vertex), max(maximum, vertex)} end
        {minimum, maximum} = List.foldl(vertices, {vertex, vertex}, min_max)
        {:ok, minimum, maximum}
    end
  end

  @doc """
  Inserts vertex into graph.

  ## Examples

      iex> vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(vertices, edges)
      iex> {:ok, graph} = InductiveGraph.insert_vertex(graph, {4, "d"})
      iex> {:ok, context, _graph} = InductiveGraph.decompose_by_vertex(graph, 4)
      iex> context
      {[], 4, "d", []}

  """
  @spec insert_vertex(t, {vertex, label}) :: {:ok, t} | :error
  def insert_vertex(%Graph{internal: map}, {vertex, label}) do
    case Map.has_key?(map, vertex) do
      true -> :error
      false -> {:ok, %Graph{internal: Map.put(map, vertex, {%{}, label, %{}})}}
    end
  end

  @doc """
  Lists all edges in graph.

  ## Examples

      iex> vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(vertices, edges)
      iex> graph |> InductiveGraph.list_edges() |> Enum.sort()
      [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]

  """
  @spec list_edges(t) :: [{vertex, vertex, label}]
  def list_edges(%Graph{internal: map}) do
    for {source, {_predecessors, _label, successors}} <- Map.to_list(map),
        {target, labels} <- Map.to_list(successors),
        label <- labels do
      {source, target, label}
    end
  end

  @doc """
  Checks if two graphs are equal.

  ## Examples

      iex> vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> {:ok, graph1} = InductiveGraph.make_graph(vertices, [])
      iex> {:ok, graph2} = InductiveGraph.insert_edges(graph1, edges)
      iex> {:ok, graph3} = InductiveGraph.make_graph(vertices, edges)
      iex> InductiveGraph.equal?(graph1, graph2)
      false
      iex> InductiveGraph.equal?(graph2, graph3)
      true

  """
  @spec equal?(t, t) :: boolean
  def equal?(graph1, graph2) do
    vertices1 = graph1 |> InductiveGraph.list_vertices() |> Enum.sort()
    edges1 = graph1 |> InductiveGraph.list_edges() |> Enum.sort()
    vertices2 = graph2 |> InductiveGraph.list_vertices() |> Enum.sort()
    edges2 = graph2 |> InductiveGraph.list_edges() |> Enum.sort()
    (vertices1 == vertices2) and (edges1 == edges2)
  end

  @doc """
  Updates graph by joining with a context.

  ## Examples

      iex> vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> {:ok, graph1} = InductiveGraph.make_graph(vertices, edges)
      iex> {:ok, context, graph2} = InductiveGraph.decompose_by_vertex(graph1, 3)
      iex> {:ok, graph3} = InductiveGraph.compose_context(graph2, context)
      iex> InductiveGraph.equal?(graph1, graph3)
      true

  """
  @spec compose_context(t, context) :: {:ok, t} | :error
  def compose_context(graph, context)
  def compose_context(%Graph{internal: map}, {predecessors, vertex, label, successors}) do
    internal_predecessors = convert_to_internal_adjacents(predecessors)
    internal_successors = convert_to_internal_adjacents(successors)
    map = Map.put(map, vertex, {internal_predecessors, label, internal_successors})

    update =
      fn
        {adjacent_vertex, labels}, {:ok, map}, location -> update_internal_map(map, adjacent_vertex, {vertex, labels}, location)
        _adjacents, :error, _location -> :error
      end

    with {:ok, map} <- List.foldl(Map.to_list(internal_predecessors), {:ok, map}, &update.(&1, &2, :successors)),
         {:ok, map} <- List.foldl(Map.to_list(internal_successors), {:ok, map}, &update.(&1, &2, :predecessors)) do
      {:ok, %Graph{internal: map}}
    else
      _error -> :error
    end
  end

  # Converts adjacents to internal adjacents.
  @spec convert_to_internal_adjacents(adj) :: iadj
  defp convert_to_internal_adjacents(adjacents) do
    convert = fn {label, vertex}, internal_adjacents -> update_internal_adjacents(internal_adjacents, vertex, [label]) end
    List.foldl(adjacents, %{}, convert)
  end
end
