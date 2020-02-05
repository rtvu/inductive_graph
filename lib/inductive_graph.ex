defmodule InductiveGraph do
  @moduledoc """
  Functions that work on graphs as an inductive data structure.
  """

  alias InductiveGraph, as: Graph
  alias InductiveGraph.Internal

  @opaque t :: %__MODULE__{}
  @type value :: term
  @type vertex :: integer
  @type tagged_vertex :: {vertex, value}
  @type tagged_edge :: {from_vertex :: vertex, to_vertex :: vertex, value}
  @type adjacents :: [{value, vertex}]
  @type context :: {predecessors :: adjacents, vertex, value, successors :: adjacents}

  defstruct [
    internal: Internal.empty_graph(),
    vertices: []
  ]

  defimpl Inspect do
    def inspect(%Graph{}, _opts) do
      "#InductiveGraph<>"
    end
  end

  @doc ~S"""
  Pretty prints inductive representation of `graph`.

  If `count` is provided, then up to `count` number of contexts will be shown.

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
  def pretty_print(%Graph{internal: internal}, count) do
    Internal.pretty_print(internal, count)
  end

  @doc """
  Creates an empty graph.

  ## Examples

      iex> graph = InductiveGraph.empty_graph()
      #InductiveGraph<>
      iex> InductiveGraph.pretty_print(graph)
      "| Empty"

  """
  @spec empty_graph() :: t
  def empty_graph(), do: %Graph{}

  @doc """
  Determines if `graph` is empty.

  ## Examples

      iex> graph = InductiveGraph.empty_graph()
      iex> InductiveGraph.empty?(graph)
      true
      iex> {:ok, graph} = InductiveGraph.make_graph([{1, 1}], [])
      iex> InductiveGraph.empty?(graph)
      false

  """
  @spec empty?(t) :: boolean
  def empty?(graph)
  def empty?(%Graph{vertices: []}), do: true
  def empty?(%Graph{}), do: false

  @doc """
  Creates a graph from lists `vertices` and `edges`.

  ## Examples

      iex> vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(vertices, edges)
      iex> InductiveGraph.list_vertices(graph) |> Enum.sort()
      [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> InductiveGraph.list_edges(graph) |> Enum.sort()
      [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]

  """
  @spec make_graph([tagged_vertex], [tagged_edge]) :: {:ok, t} | :error
  def make_graph(vertices, edges) do
    with {:ok, internal} <- Internal.make_graph(vertices, edges) do
      vertices = Internal.list_vertices(internal)
      {:ok, %Graph{internal: internal, vertices: vertices}}
    else
      _error -> :error
    end
  end

  @doc ~S"""
  Inserts `edges` into `graph`.

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
  @spec insert_edges(t, [tagged_edge]) :: {:ok, t} | :error
  def insert_edges(graph, edges)
  def insert_edges(graph = %Graph{internal: internal}, edges) do
    with {:ok, internal} <- Internal.insert_edges(internal, edges) do
      {:ok, %{graph | internal: internal}}
    else
      _error -> :error
    end
  end

  @doc ~S"""
  Inserts `edge` into `graph`.

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
  @spec insert_edge(t, tagged_edge) :: {:ok, t} | :error
  def insert_edge(graph, edge)
  def insert_edge(graph = %Graph{internal: internal}, edge) do
    with {:ok, internal} <- Internal.insert_edge(internal, edge) do
      {:ok, %{graph | internal: internal}}
    else
      _error -> :error
    end
  end

  @doc """
  Decomposes `graph` into the context containing `vertex` and the remaining
  graph.

  ## Examples

      iex> vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(vertices, edges)
      iex> {:ok, context1, graph} = InductiveGraph.decompose(graph, 3)
      iex> context1
      {[{"down", 2}], 3, "c", [{"up", 1}]}
      iex> {:ok, context2, graph} = InductiveGraph.decompose(graph, 2)
      iex> context2
      {[{"right", 1}], 2, "b", [{"left", 1}]}
      iex> {:ok, context3, graph} = InductiveGraph.decompose(graph, 1)
      iex> context3
      {[], 1, "a", []}
      iex> InductiveGraph.empty?(graph)
      true

  """
  @spec decompose(t, vertex) :: {:ok, context, t} | :error
  def decompose(graph, vertex)
  def decompose(%Graph{internal: internal, vertices: vertices}, vertex) do
    with {:ok, context, internal} <- Internal.decompose(internal, vertex) do
      vertex = {vertex, elem(context, 2)}
      vertices = List.delete(vertices, vertex)
      {:ok, context, %Graph{internal: internal, vertices: vertices}}
    else
      _error -> :error
    end
  end

  @doc """
  Decomposes `graph` into an arbitrary context and the remaining graph.

  ## Examples

      iex> vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(vertices, edges)
      iex> InductiveGraph.count_vertices(graph)
      3
      iex> {:ok, _context, graph} = InductiveGraph.decompose(graph)
      iex> InductiveGraph.count_vertices(graph)
      2

  """
  @spec decompose(t) :: {:ok, context, t} | :error
  def decompose(graph)
  def decompose(%Graph{vertices: []}), do: :error
  def decompose(graph = %Graph{vertices: vertices}) do
    [{vertex, _value} | _rest] = vertices |> Enum.sort()
    decompose(graph, vertex)
  end

  @doc """
  Lists all vertices in `graph`.

  ## Examples

      iex> vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(vertices, [])
      iex> graph |> InductiveGraph.list_vertices() |> Enum.sort()
      [{1, "a"}, {2, "b"}, {3, "c"}]

  """
  @spec list_vertices(t) :: [tagged_vertex]
  def list_vertices(graph)
  def list_vertices(%Graph{vertices: vertices}) do
    vertices |> Enum.sort()
  end

  @doc """
  Counts number of vertices in `graph`.

  ## Examples

      iex> vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(vertices, [])
      iex> InductiveGraph.count_vertices(graph)
      3

  """
  @spec count_vertices(t) :: non_neg_integer
  def count_vertices(graph)
  def count_vertices(%Graph{vertices: vertices}) do
    length(vertices)
  end

  @doc """
  Gets range of vertex values in `graph`.

  ## Examples

      iex> vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(vertices, [])
      iex> InductiveGraph.vertex_range(graph)
      {:ok, 1, 3}

  """
  @spec vertex_range(t) :: {:ok, min :: integer, max :: integer} | :error
  def vertex_range(graph)
  def vertex_range(%Graph{internal: internal}) do
    Internal.vertex_range(internal)
  end

  @doc ~S"""
  Inserts `vertices` into `graph`.

  ## Examples

      iex> vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(vertices, edges)
      iex> {:ok, graph} = InductiveGraph.insert_vertices(graph, [{4, "d"}, {5, "e"}])
      iex> InductiveGraph.pretty_print(graph, 2) <> "\n"
      ~s'''
      | {[], 5, "e", []}
      & {[], 4, "d", []}
      & InductiveGraph
      '''

  """
  @spec insert_vertices(t, [tagged_vertex]) :: {:ok, t} | :error
  def insert_vertices(graph, vertices)
  def insert_vertices(%Graph{internal: internal, vertices: vertices}, new_vertices) do
    with {:ok, internal} <- Internal.insert_vertices(internal, new_vertices) do
      vertices = new_vertices ++ vertices
      {:ok, %Graph{internal: internal, vertices: vertices}}
    else
      _error -> :error
    end
  end

  @doc """
  Inserts `vertex` into `graph`.

  ## Examples

      iex> vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(vertices, edges)
      iex> {:ok, graph} = InductiveGraph.insert_vertex(graph, {4, "d"})
      iex> {:ok, context, _graph} = InductiveGraph.decompose(graph, 4)
      iex> context
      {[], 4, "d", []}

  """
  @spec insert_vertex(t, tagged_vertex) :: {:ok, t} | :error
  def insert_vertex(graph, vertex)
  def insert_vertex(%Graph{internal: internal, vertices: vertices}, vertex) do
    with {:ok, internal} <- Internal.insert_vertex(internal, vertex) do
      vertices = [vertex | vertices]
      {:ok, %Graph{internal: internal, vertices: vertices}}
    else
      _error -> :error
    end
  end

  @doc """
  Lists all edges in `graph`.

  ## Examples

      iex> vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(vertices, edges)
      iex> graph |> InductiveGraph.list_edges() |> Enum.sort()
      [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]

  """
  @spec list_edges(t) :: [tagged_edge]
  def list_edges(graph)
  def list_edges(%Graph{internal: internal}) do
    Internal.list_edges(internal)
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
    vertices1 = Map.fetch!(graph1, :vertices) |> Enum.sort()
    edges1 = graph1 |> InductiveGraph.list_edges() |> Enum.sort()
    vertices2 = Map.fetch!(graph2, :vertices) |> Enum.sort()
    edges2 = graph2 |> InductiveGraph.list_edges() |> Enum.sort()
    (vertices1 == vertices2) and (edges1 == edges2)
  end

  @doc """
  Merges `context` into `graph`.

  ## Examples

      iex> vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> {:ok, graph1} = InductiveGraph.make_graph(vertices, edges)
      iex> {:ok, context, graph2} = InductiveGraph.decompose(graph1, 3)
      iex> {:ok, graph3} = InductiveGraph.merge(graph2, context)
      iex> InductiveGraph.equal?(graph1, graph3)
      true

  """
  @spec merge(t, context) :: {:ok, t} | :error
  def merge(graph, context)
  def merge(%Graph{internal: internal, vertices: vertices}, context = {_predecessors, vertex, value, _successors}) do
    with {:ok, internal} <- Internal.merge(internal, context) do
      vertices = [{vertex, value} | vertices]
      {:ok, %Graph{internal: internal, vertices: vertices}}
    else
      _error -> :error
    end
  end
end
