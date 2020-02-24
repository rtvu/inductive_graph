defmodule InductiveGraph do
  @moduledoc """
  Functions that work on graphs as an inductive data structure.
  """

  alias InductiveGraph, as: Graph
  alias InductiveGraph.Internal
  alias InductiveGraph.Utilities

  @opaque t :: %__MODULE__{}
  @type value :: term
  @type edge_value :: value
  @type vertex_value :: value
  @type vertex :: integer
  @type neighbor :: vertex
  @type edge :: {from_vertex :: vertex, to_vertex :: vertex}
  @type tagged_vertex :: {vertex, vertex_value}
  @type tagged_edge :: {from_vertex :: vertex, to_vertex :: vertex, edge_value}
  @type adjacents :: [{edge_value, neighbor}]
  @type predecessors :: adjacents
  @type successors :: adjacents
  @type context :: {predecessors, vertex, vertex_value, successors}

  defstruct [
    internal: Internal.empty_graph(),
  ]

  defimpl Inspect do
    def inspect(%Graph{}, _opts) do
      "#InductiveGraph<>"
    end
  end

  # Wraps `graph` to `%InductiveGraph{}`.
  @spec wrap(Internal.t) :: t
  defp wrap(graph), do: %Graph{internal: graph}

  # Wraps `graph` in `position` of `fallible` to `%InductiveGraph{}`.
  #
  # Fallible is either `:error` or an `tuple` with first element `:ok`.
  @spec wrap_fallible(tuple | :error, non_neg_integer) :: tuple | :error
  defp wrap_fallible(fallible, position) do
    with true <- is_tuple(fallible),
         true <- position > 0,
         :ok <- elem(fallible, 0) do
      Utilities.tuple_update_position!(fallible, position, &wrap/1)
    else
      _error -> :error
    end
  end

  @doc ~S"""
  Pretty prints inductive representation of `graph`.

  If `count` is provided, then up to `count` number of contexts will be shown.

  ## Examples

      iex> tagged_vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> tagged_edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(tagged_vertices, tagged_edges)
      iex> InductiveGraph.pretty_print(graph) <> "\n"
      ~s'''
      | {[{"down", 2}], 3, "c", [{"up", 1}]}
      & {[{"right", 1}], 2, "b", [{"left", 1}]}
      & {[], 1, "a", []}
      & Empty
      '''

  """
  @doc inspection: true
  @spec pretty_print(t, integer) :: String.t
  def pretty_print(graph, count \\ -1)
  def pretty_print(%Graph{internal: graph}, count) do
    Internal.pretty_print(graph, count)
  end

  @doc """
  Creates an empty graph.

  ## Examples

      iex> graph = InductiveGraph.empty_graph()
      #InductiveGraph<>
      iex> InductiveGraph.pretty_print(graph)
      "| Empty"

  """
  @doc construction: true
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
  @doc inspection: true
  @spec empty?(t) :: boolean
  def empty?(graph)
  def empty?(%Graph{internal: graph}), do: Internal.empty?(graph)

  @doc """
  Creates a graph from lists `tagged_vertices` and `tagged_edges`.

  ## Examples

      iex> tagged_vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> tagged_edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(tagged_vertices, tagged_edges)
      iex> graph |> InductiveGraph.list_tagged_vertices() |> Enum.sort()
      [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> graph |> InductiveGraph.list_tagged_edges() |> Enum.sort()
      [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]

  """
  @doc construction: true
  @spec make_graph([tagged_vertex], [tagged_edge]) :: {:ok, t} | :error
  def make_graph(tagged_vertices, tagged_edges) do
    Internal.make_graph(tagged_vertices, tagged_edges)
    |> wrap_fallible(1)
  end

  @doc ~S"""
  Inserts `tagged_edge` into `graph`.

  ## Examples

      iex> tagged_vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> tagged_edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}]
      iex> new_tagged_edge = {3, 1, "up"}
      iex> {:ok, graph} = InductiveGraph.make_graph(tagged_vertices, tagged_edges)
      iex> {:ok, graph} = InductiveGraph.insert_tagged_edge(graph, new_tagged_edge)
      iex> InductiveGraph.pretty_print(graph) <> "\n"
      ~s'''
      | {[{"down", 2}], 3, "c", [{"up", 1}]}
      & {[{"right", 1}], 2, "b", [{"left", 1}]}
      & {[], 1, "a", []}
      & Empty
      '''

  """
  @doc construction: true
  @spec insert_tagged_edge(t, tagged_edge) :: {:ok, t} | :error
  def insert_tagged_edge(graph, tagged_edge)
  def insert_tagged_edge(%Graph{internal: graph}, tagged_edge) do
    Internal.insert_tagged_edge(graph, tagged_edge)
    |> wrap_fallible(1)
  end

  @doc ~S"""
  Inserts `tagged_edges` into `graph`.

  ## Examples

  iex> tagged_vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
  iex> tagged_edges = []
  iex> new_tagged_edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
  iex> {:ok, graph} = InductiveGraph.make_graph(tagged_vertices, tagged_edges)
  iex> {:ok, graph} = InductiveGraph.insert_tagged_edges(graph, new_tagged_edges)
  iex> InductiveGraph.pretty_print(graph) <> "\n"
  ~s'''
  | {[{"down", 2}], 3, "c", [{"up", 1}]}
  & {[{"right", 1}], 2, "b", [{"left", 1}]}
  & {[], 1, "a", []}
  & Empty
  '''

  """
  @doc construction: true
  @spec insert_tagged_edges(t, [tagged_edge]) :: {:ok, t} | :error
  def insert_tagged_edges(graph, tagged_edges)
  def insert_tagged_edges(%Graph{internal: graph}, tagged_edges) do
    Internal.insert_tagged_edges(graph, tagged_edges)
    |> wrap_fallible(1)
  end

  @doc """
  Inserts `tagged_vertex` into `graph`.

  ## Examples

      iex> tagged_vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> tagged_edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> new_tagged_vertex = {4, "d"}
      iex> {:ok, graph} = InductiveGraph.make_graph(tagged_vertices, tagged_edges)
      iex> {:ok, graph} = InductiveGraph.insert_tagged_vertex(graph, new_tagged_vertex)
      iex> {:ok, context, _graph} = InductiveGraph.decompose(graph, 4)
      iex> context
      {[], 4, "d", []}

  """
  @doc construction: true
  @spec insert_tagged_vertex(t, tagged_vertex) :: {:ok, t} | :error
  def insert_tagged_vertex(graph, tagged_vertex)
  def insert_tagged_vertex(%Graph{internal: graph}, tagged_vertex) do
    Internal.insert_tagged_vertex(graph, tagged_vertex)
    |> wrap_fallible(1)
  end

  @doc ~S"""
  Inserts `tagged_vertices` into `graph`.

  ## Examples

      iex> tagged_vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> tagged_edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> new_tagged_vertices = [{4, "d"}, {5, "e"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(tagged_vertices, tagged_edges)
      iex> {:ok, graph} = InductiveGraph.insert_tagged_vertices(graph, new_tagged_vertices)
      iex> InductiveGraph.pretty_print(graph, 2) <> "\n"
      ~s'''
      | {[], 5, "e", []}
      & {[], 4, "d", []}
      & InductiveGraph
      '''

  """
  @doc construction: true
  @spec insert_tagged_vertices(t, [tagged_vertex]) :: {:ok, t} | :error
  def insert_tagged_vertices(graph, tagged_vertices)
  def insert_tagged_vertices(%Graph{internal: graph}, tagged_vertices) do
    Internal.insert_tagged_vertices(graph, tagged_vertices)
    |> wrap_fallible(1)
  end

  @doc """
  Decomposes `graph` into the context containing `vertex` and the remaining
  graph.

  ## Examples

      iex> tagged_vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> tagged_edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(tagged_vertices, tagged_edges)
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
  @doc destruction: true
  @spec decompose(t, vertex) :: {:ok, context, t} | :error
  def decompose(graph, vertex)
  def decompose(%Graph{internal: graph}, vertex) do
    Internal.decompose(graph, vertex)
    |> wrap_fallible(2)
  end

  @doc """
  Decomposes `graph` into an arbitrary context and the remaining graph.
  ## Examples
      iex> tagged_vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> tagged_edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(tagged_vertices, tagged_edges)
      iex> InductiveGraph.count_vertices(graph)
      3
      iex> {:ok, _context, graph} = InductiveGraph.decompose(graph)
      iex> InductiveGraph.count_vertices(graph)
      2
  """
  @doc destruction: true
  @spec decompose(t) :: {:ok, context, t} | :error
  def decompose(graph)
  def decompose(graph = %Graph{internal: internal}) do
    [{vertex, _vertex_value} | _tagged_vertices] = Internal.list_tagged_vertices(internal) |> Enum.sort() |> Enum.reverse()
    decompose(graph, vertex)
  end

  @doc """
  Merges `context` into `graph`.

  ## Examples

      iex> tagged_vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> tagged_edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> {:ok, graph1} = InductiveGraph.make_graph(tagged_vertices, tagged_edges)
      iex> {:ok, context, graph2} = InductiveGraph.decompose(graph1, 3)
      iex> {:ok, graph3} = InductiveGraph.merge(graph2, context)
      iex> InductiveGraph.equal?(graph1, graph3)
      true

  """
  @doc construction: true
  @spec merge(t, context) :: {:ok, t} | :error
  def merge(graph, context)
  def merge(%Graph{internal: graph}, context) do
    Internal.merge(graph, context)
    |> wrap_fallible(1)
  end

  @doc """
  Lists all tagged vertices in `graph`.

  ## Examples

      iex> tagged_vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(tagged_vertices, [])
      iex> graph |> InductiveGraph.list_tagged_vertices() |> Enum.sort()
      [{1, "a"}, {2, "b"}, {3, "c"}]

  """
  @doc inspection: true
  @spec list_tagged_vertices(t) :: [tagged_vertex]
  def list_tagged_vertices(graph)
  def list_tagged_vertices(%Graph{internal: graph}) do
    Internal.list_tagged_vertices(graph)
  end

  @doc """
  Counts number of vertices in `graph`.

  ## Examples

      iex> tagged_vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(tagged_vertices, [])
      iex> InductiveGraph.count_vertices(graph)
      3

  """
  @doc inspection: true
  @spec count_vertices(t) :: non_neg_integer
  def count_vertices(graph)
  def count_vertices(%Graph{internal: graph}) do
    Internal.count_vertices(graph)
  end

  @doc """
  Lists all tagged edges in `graph`.

  ## Examples

      iex> tagged_vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> tagged_edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(tagged_vertices, tagged_edges)
      iex> graph |> InductiveGraph.list_tagged_edges() |> Enum.sort()
      [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]

  """
  @doc inspection: true
  @spec list_tagged_edges(t) :: [tagged_edge]
  def list_tagged_edges(graph)
  def list_tagged_edges(%Graph{internal: graph}) do
    Internal.list_tagged_edges(graph)
  end

  @doc """
  Counts number of edges in `graph`.

  ## Examples

      iex> tagged_vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> tagged_edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(tagged_vertices, tagged_edges)
      iex> graph |> InductiveGraph.count_edges()
      4

  """
  @doc inspection: true
  @spec count_edges(t) :: non_neg_integer
  def count_edges(graph)
  def count_edges(%Graph{internal: graph}) do
    graph |> Internal.list_tagged_edges() |> length()
  end

  @doc """
  Gets range of vertex values in `graph`.

  ## Examples

      iex> tagged_vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(tagged_vertices, [])
      iex> InductiveGraph.vertex_range(graph)
      {:ok, 1, 3}

  """
  @doc inspection: true
  @spec vertex_range(t) :: {:ok, mininum :: integer, maximum :: integer} | :error
  def vertex_range(graph)
  def vertex_range(%Graph{internal: graph}) do
    Internal.vertex_range(graph)
  end

  @doc """
  Determines if `vertex` is in `graph`.

  ## Examples

      iex> tagged_vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> edges = []
      iex> {:ok, graph} = InductiveGraph.make_graph(tagged_vertices, edges)
      iex> InductiveGraph.has_vertex?(graph, 3)
      true
      iex> InductiveGraph.has_vertex?(graph, 4)
      false

  """
  @doc inspection: true
  @spec has_vertex?(t, vertex) :: boolean
  def has_vertex?(graph, vertex)
  def has_vertex?(%Graph{internal: graph}, vertex), do: Internal.has_vertex?(graph, vertex)

  @doc """
  Determines if `edge` is in `graph`.

  ## Examples

      iex> tagged_vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> tagged_edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(tagged_vertices, tagged_edges)
      iex> InductiveGraph.has_edge?(graph, {1, 2})
      true
      iex> InductiveGraph.has_edge?(graph, {1, 3})
      false

  """
  @doc inspection: true
  @spec has_edge?(t, edge) :: boolean
  def has_edge?(graph, edge)
  def has_edge?(%Graph{internal: graph}, edge), do: Internal.has_edge?(graph, edge)

  @doc """
  Checks if two graphs are equal.

  ## Examples

      iex> tagged_vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> tagged_edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> {:ok, graph1} = InductiveGraph.make_graph(tagged_vertices, [])
      iex> {:ok, graph2} = InductiveGraph.insert_tagged_edges(graph1, tagged_edges)
      iex> {:ok, graph3} = InductiveGraph.make_graph(tagged_vertices, tagged_edges)
      iex> InductiveGraph.equal?(graph1, graph2)
      false
      iex> InductiveGraph.equal?(graph2, graph3)
      true

  """
  @doc inspection: true
  @spec equal?(t, t) :: boolean
  def equal?(graph1, graph2) do
    vertices1 = graph1 |> list_tagged_vertices() |> Enum.sort()
    edges1 = graph1 |> list_tagged_edges() |> Enum.sort()
    vertices2 = graph2 |> list_tagged_vertices() |> Enum.sort()
    edges2 = graph2 |> list_tagged_edges() |> Enum.sort()
    (vertices1 == vertices2) and (edges1 == edges2)
  end

  @doc """
  Applies `function` to every context in `graph`.

  ## Examples

      iex> tagged_vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> tagged_edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> function = fn context = {_p, _v, value, _s} -> put_elem(context, 2, String.duplicate(value, 5)) end
      iex> {:ok, graph} = InductiveGraph.make_graph(tagged_vertices, tagged_edges)
      iex> graph = InductiveGraph.map_graph(graph, function)
      iex> {:ok, context, _graph} = InductiveGraph.decompose(graph, 3)
      iex> context
      {[{"down", 2}], 3, "ccccc", [{"up", 1}]}

  """
  @doc update: true
  @spec map_graph(t, (context -> context)) :: t
  def map_graph(graph, function)
  def map_graph(%Graph{internal: graph}, function) do
    Internal.map_graph(graph, function)
    |> wrap()
  end

  @doc """
  Applies `function` to every vertex value in `graph`.

  ## Examples

      iex> tagged_vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> tagged_edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> function = &String.duplicate(&1, 5)
      iex> {:ok, graph} = InductiveGraph.make_graph(tagged_vertices, tagged_edges)
      iex> graph = InductiveGraph.map_vertices(graph, function)
      iex> {:ok, context, _graph} = InductiveGraph.decompose(graph, 3)
      iex> context
      {[{"down", 2}], 3, "ccccc", [{"up", 1}]}

  """
  @doc update: true
  @spec map_vertices(t, (vertex_value -> vertex_value)) :: t
  def map_vertices(graph, function)
  def map_vertices(%Graph{internal: graph}, function) do
    Internal.map_vertices(graph, function)
    |> wrap()
  end

  @doc """
  Applies `function` to every edge value in `graph`.

  ## Examples

      iex> tagged_vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> tagged_edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> function = &String.reverse(&1)
      iex> {:ok, graph} = InductiveGraph.make_graph(tagged_vertices, tagged_edges)
      iex> graph = InductiveGraph.map_edges(graph, function)
      iex> {:ok, context, _graph} = InductiveGraph.decompose(graph, 3)
      iex> context
      {[{"nwod", 2}], 3, "c", [{"pu", 1}]}

  """
  @doc update: true
  @spec map_edges(t, (edge_value -> edge_value)) :: t
  def map_edges(graph, function)
  def map_edges(%Graph{internal: graph}, function) do
    Internal.map_edges(graph, function)
    |> wrap()
  end

  @doc """
  Applies `vertex_function` to every vertex value and `edge_function` to every
  edge value in `graph`.

  ## Examples

      iex> tagged_vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> tagged_edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> vertex_function = &String.duplicate(&1, 5)
      iex> edge_function = &String.reverse(&1)
      iex> {:ok, graph} = InductiveGraph.make_graph(tagged_vertices, tagged_edges)
      iex> graph = InductiveGraph.map_vertices_and_edges(graph, vertex_function, edge_function)
      iex> {:ok, context, _graph} = InductiveGraph.decompose(graph, 3)
      iex> context
      {[{"nwod", 2}], 3, "ccccc", [{"pu", 1}]}

  """
  @doc update: true
  @spec map_vertices_and_edges(t, (vertex_value -> vertex_value), (edge_value -> edge_value)) :: t
  def map_vertices_and_edges(graph, vertex_function, edge_function)
  def map_vertices_and_edges(%Graph{internal: graph}, vertex_function, edge_function) do
    Internal.map_vertices_and_edges(graph, vertex_function, edge_function)
    |> wrap()
  end

  @doc """
  Folds `function` over `graph` with starting value `accumulator`.

  ## Examples

      iex> tagged_vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> tagged_edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> function = fn {_p, vertex, _v, _s}, result -> vertex + result end
      iex> {:ok, graph} = InductiveGraph.make_graph(tagged_vertices, tagged_edges)
      iex> InductiveGraph.unordered_fold(graph, 0, function)
      6

  """
  @doc update: true
  @spec unordered_fold(t, term, (context, term -> term)) :: term
  def unordered_fold(graph, accumulator, function) do
    get_vertex = fn {vertex, _vertex_value} -> vertex end
    vertices = graph |> list_tagged_vertices() |> Enum.map(get_vertex)
    unordered_fold(graph, accumulator, function, vertices)
  end

  # Folds `function` over `graph` with starting value `accumulator`.
  @spec unordered_fold(t, term, (context, term -> term), [vertex]) :: term
  defp unordered_fold(_graph, accumulator, _function, []), do: accumulator
  defp unordered_fold(graph, accumulator, function, [vertex | vertices]) do
    {:ok, context, graph} = decompose(graph, vertex)
    accumulator = function.(context, accumulator)
    unordered_fold(graph, accumulator, function, vertices)
  end

  @doc """
  Deletes `vertex` from `graph`.

  ## Examples

      iex> tagged_vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> tagged_edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(tagged_vertices, tagged_edges)
      iex> {:ok, graph} = InductiveGraph.delete_vertex(graph, 3)
      iex> InductiveGraph.has_vertex?(graph, 3)
      false

  """
  @doc destruction: true
  @spec delete_vertex(t, vertex) :: {:ok, t} | :error
  def delete_vertex(graph, vertex) do
    with {:ok, _context, graph} <- decompose(graph, vertex) do
      {:ok, graph}
    else
      _error -> :error
    end
  end

  @doc """
  Deletes `vertices` from `graph`.

  ## Examples

      iex> tagged_vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> tagged_edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(tagged_vertices, tagged_edges)
      iex> {:ok, graph} = InductiveGraph.delete_vertices(graph, [2, 3])
      iex> InductiveGraph.has_vertex?(graph, 3)
      false

  """
  @doc destruction: true
  @spec delete_vertices(t, [vertex]) :: {:ok, t} | :error
  def delete_vertices(graph, vertices) do
    delete =
      fn
        vertex, {:ok, graph} -> delete_vertex(graph, vertex)
        _vertex, :error -> :error
      end

    List.foldl(vertices, {:ok, graph}, delete)
  end

  @doc ~S"""
  Deletes `edge` from `graph`.

  ## Examples

      iex> tagged_vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> tagged_edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> edge = {1, 2}
      iex> {:ok, graph} = InductiveGraph.make_graph(tagged_vertices, tagged_edges)
      iex> {:ok, graph} = InductiveGraph.delete_edge(graph, edge)
      iex> InductiveGraph.pretty_print(graph) <> "\n"
      ~s'''
      | {[{"down", 2}], 3, "c", [{"up", 1}]}
      & {[], 2, "b", [{"left", 1}]}
      & {[], 1, "a", []}
      & Empty
      '''

  """
  @doc destruction: true
  @spec delete_edge(t, edge) :: {:ok, t} | :error
  def delete_edge(graph, edge)
  def delete_edge(graph, {from_vertex, to_vertex}) do
    predicate = fn {_edge_value, neighbor} -> neighbor == to_vertex end
    with true <- has_vertex?(graph, from_vertex),
         {:ok, context, graph} <- decompose(graph, from_vertex),
         {predecessors, vertex, vertex_value, successors} = context,
         successors = Enum.reject(successors, predicate),
         context = {predecessors, vertex, vertex_value, successors} do
      merge(graph, context)
    else
      _error -> :error
    end
  end

  @doc ~S"""
  Deletes `edges` from `graph`.

  ## Examples

      iex> tagged_vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> tagged_edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> edges = [{1, 2}, {2, 1}]
      iex> {:ok, graph} = InductiveGraph.make_graph(tagged_vertices, tagged_edges)
      iex> {:ok, graph} = InductiveGraph.delete_edges(graph, edges)
      iex> InductiveGraph.pretty_print(graph) <> "\n"
      ~s'''
      | {[{"down", 2}], 3, "c", [{"up", 1}]}
      & {[], 2, "b", []}
      & {[], 1, "a", []}
      & Empty
      '''

  """
  @doc destruction: true
  @spec delete_edges(t, [edge]) :: {:ok, t} | :error
  def delete_edges(graph, edges) do
    delete =
      fn
        edge, {:ok, graph} -> delete_edge(graph, edge)
        _vertex, :error -> :error
      end

    List.foldl(edges, {:ok, graph}, delete)
  end
end
