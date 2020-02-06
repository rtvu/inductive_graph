defmodule InductiveGraph do
  @moduledoc """
  Functions that work on graphs as an inductive data structure.
  """

  alias InductiveGraph, as: Graph
  alias InductiveGraph.Internal
  alias InductiveGraph.Utilities

  @opaque t :: %__MODULE__{}
  @type value :: term
  @type vertex :: integer
  @type edge :: {from_vertex :: vertex, to_vertex :: vertex}
  @type tagged_vertex :: {vertex, value}
  @type tagged_edge :: {from_vertex :: vertex, to_vertex :: vertex, value}
  @type adjacents :: [{value, vertex}]
  @type context :: {predecessors :: adjacents, vertex, value, successors :: adjacents}

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
  # Fallible is either `:error` or an n-tuple with first element `:ok`.
  @spec wrap_fallible(tuple | :error, non_neg_integer) :: tuple | :error
  defp wrap_fallible(fallible, position) do
    with true <- is_tuple(fallible),
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
  def empty?(%Graph{internal: graph}), do: Internal.empty?(graph)

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
    Internal.make_graph(vertices, edges)
    |> wrap_fallible(1)
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
  def insert_edges(%Graph{internal: graph}, edges) do
    Internal.insert_edges(graph, edges)
    |> wrap_fallible(1)
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
  def insert_edge(%Graph{internal: graph}, edge) do
    Internal.insert_edge(graph, edge)
    |> wrap_fallible(1)
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
  def decompose(%Graph{internal: graph}, vertex) do
    Internal.decompose(graph, vertex)
    |> wrap_fallible(2)
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
  def decompose(graph = %Graph{internal: internal}) do
    [{vertex, _value} | _vertices] = Internal.list_vertices(internal) |> Enum.sort() |> Enum.reverse()
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
  def list_vertices(%Graph{internal: graph}) do
    Internal.list_vertices(graph)
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
  def count_vertices(%Graph{internal: graph}) do
    Internal.count_vertices(graph)
  end

  @doc """
  Counts number of edges in `graph`.

  ## Examples

      iex> vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(vertices, edges)
      iex> graph |> InductiveGraph.count_edges()
      4

  """
  @spec count_edges(t) :: non_neg_integer
  def count_edges(graph)
  def count_edges(%Graph{internal: graph}) do
    graph |> Internal.list_edges() |> length()
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
  def vertex_range(%Graph{internal: graph}) do
    Internal.vertex_range(graph)
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
  def insert_vertices(%Graph{internal: graph}, vertices) do
    Internal.insert_vertices(graph, vertices)
    |> wrap_fallible(1)
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
  def insert_vertex(%Graph{internal: graph}, vertex) do
    Internal.insert_vertex(graph, vertex)
    |> wrap_fallible(1)
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
  def list_edges(%Graph{internal: graph}) do
    Internal.list_edges(graph)
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
    vertices1 = graph1 |> list_vertices() |> Enum.sort()
    edges1 = graph1 |> list_edges() |> Enum.sort()
    vertices2 = graph2 |> list_vertices() |> Enum.sort()
    edges2 = graph2 |> list_edges() |> Enum.sort()
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
  def merge(%Graph{internal: graph}, context) do
    Internal.merge(graph, context)
    |> wrap_fallible(1)
  end

  @doc """
  Applies `function` to every context in `graph`.

  ## Examples

      iex> vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> function = fn context = {_, _, value, _} -> put_elem(context, 2, String.duplicate(value, 5)) end
      iex> {:ok, graph} = InductiveGraph.make_graph(vertices, edges)
      iex> graph = InductiveGraph.map_graph(graph, function)
      iex> {:ok, context, _graph} = InductiveGraph.decompose(graph, 3)
      iex> context
      {[{"down", 2}], 3, "ccccc", [{"up", 1}]}

  """
  @spec map_graph(t, (context -> context)) :: t
  def map_graph(graph, function)
  def map_graph(%Graph{internal: graph}, function) do
    Internal.map_graph(graph, function)
    |> wrap()
  end

  @doc """
  Applies `function` to every vertex value in `graph`.

  ## Examples

      iex> vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> function = &String.duplicate(&1, 5)
      iex> {:ok, graph} = InductiveGraph.make_graph(vertices, edges)
      iex> graph = InductiveGraph.map_vertices(graph, function)
      iex> {:ok, context, _graph} = InductiveGraph.decompose(graph, 3)
      iex> context
      {[{"down", 2}], 3, "ccccc", [{"up", 1}]}

  """
  @spec map_vertices(t, (value -> value)) :: t
  def map_vertices(graph, function)
  def map_vertices(%Graph{internal: graph}, function) do
    Internal.map_vertices(graph, function)
    |> wrap()
  end

  @doc """
  Applies `function` to every edge value in `graph`.

  ## Examples

      iex> vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> function = &String.reverse(&1)
      iex> {:ok, graph} = InductiveGraph.make_graph(vertices, edges)
      iex> graph = InductiveGraph.map_edges(graph, function)
      iex> {:ok, context, _graph} = InductiveGraph.decompose(graph, 3)
      iex> context
      {[{"nwod", 2}], 3, "c", [{"pu", 1}]}

  """
  @spec map_edges(t, (value -> value)) :: t
  def map_edges(graph, function)
  def map_edges(%Graph{internal: graph}, function) do
    Internal.map_edges(graph, function)
    |> wrap()
  end

  @doc """
  Applies `vertex_function` to every vertex value and `edge_function` to every
  edge value in `graph`.

  ## Examples

      iex> vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> vertex_function = &String.duplicate(&1, 5)
      iex> edge_function = &String.reverse(&1)
      iex> {:ok, graph} = InductiveGraph.make_graph(vertices, edges)
      iex> graph = InductiveGraph.map_vertices_and_edges(graph, vertex_function, edge_function)
      iex> {:ok, context, _graph} = InductiveGraph.decompose(graph, 3)
      iex> context
      {[{"nwod", 2}], 3, "ccccc", [{"pu", 1}]}

  """
  @spec map_vertices_and_edges(t, (value -> value), (value -> value)) :: t
  def map_vertices_and_edges(graph, vertex_function, edge_function)
  def map_vertices_and_edges(%Graph{internal: graph}, vertex_function, edge_function) do
    Internal.map_vertices_and_edges(graph, vertex_function, edge_function)
    |> wrap()
  end

  @doc """
  Folds `function` over `graph`.

  ## Examples

      iex> vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> function = fn {_, vertex, _, _}, result -> vertex + result end
      iex> {:ok, graph} = InductiveGraph.make_graph(vertices, edges)
      iex> InductiveGraph.unordered_fold(graph, 0, function)
      6

  """
  @spec unordered_fold(t, term, (context, term -> term)) :: term
  def unordered_fold(graph, accumulator, function) do
    vertices = graph |> list_vertices() |> Enum.map(fn {vertex, _value} -> vertex end)
    unordered_fold(graph, accumulator, function, vertices)
  end

  @spec unordered_fold(t, term, (context, term -> term), [vertex]) :: term
  defp unordered_fold(_graph, accumulator, _function, []), do: accumulator
  defp unordered_fold(graph, accumulator, function, [vertex | vertices]) do
    {:ok, context, graph} = decompose(graph, vertex)
    accumulator = function.(context, accumulator)
    unordered_fold(graph, accumulator, function, vertices)
  end

  @doc """
  Determines if `vertex` is in `graph`.

  ## Examples

      iex> vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> edges = []
      iex> {:ok, graph} = InductiveGraph.make_graph(vertices, edges)
      iex> InductiveGraph.has_vertex?(graph, 3)
      true
      iex> InductiveGraph.has_vertex?(graph, 4)
      false

  """
  @spec has_vertex?(t, vertex) :: boolean
  def has_vertex?(graph, vertex)
  def has_vertex?(%Graph{internal: graph}, vertex), do: Internal.has_vertex?(graph, vertex)

  @doc """
  Deletes `vertex` from `graph`.

  ## Examples

      iex> vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(vertices, edges)
      iex> {:ok, graph} = InductiveGraph.delete_vertex(graph, 3)
      iex> InductiveGraph.has_vertex?(graph, 3)
      false

  """
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

      iex> vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(vertices, edges)
      iex> {:ok, graph} = InductiveGraph.delete_vertices(graph, [2, 3])
      iex> InductiveGraph.has_vertex?(graph, 3)
      false

  """
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

      iex> vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(vertices, edges)
      iex> {:ok, graph} = InductiveGraph.delete_edge(graph, {1, 2})
      iex> InductiveGraph.pretty_print(graph) <> "\n"
      ~s'''
      | {[{"down", 2}], 3, "c", [{"up", 1}]}
      & {[], 2, "b", [{"left", 1}]}
      & {[], 1, "a", []}
      & Empty
      '''

  """
  @spec delete_edge(t, edge) :: {:ok, t} | :error
  def delete_edge(graph, edge)
  def delete_edge(graph, {from_vertex, to_vertex}) do
    predicate = fn {_value, vertex} -> vertex == to_vertex end
    with true <- has_vertex?(graph, from_vertex),
         true <- has_vertex?(graph, to_vertex),
         {:ok, context, graph} <- decompose(graph, from_vertex),
         {predecessors, vertex, value, successors} = context,
         successors = Enum.reject(successors, predicate),
         context = {predecessors, vertex, value, successors} do
      merge(graph, context)
    else
      _error -> :error
    end
  end

  @doc ~S"""
  Deletes `edges` from `graph`.

  ## Examples

      iex> vertices = [{1, "a"}, {2, "b"}, {3, "c"}]
      iex> edges = [{1, 2, "right"}, {2, 1, "left"}, {2, 3, "down"}, {3, 1, "up"}]
      iex> {:ok, graph} = InductiveGraph.make_graph(vertices, edges)
      iex> {:ok, graph} = InductiveGraph.delete_edges(graph, [{1, 2}, {2, 1}])
      iex> InductiveGraph.pretty_print(graph) <> "\n"
      ~s'''
      | {[{"down", 2}], 3, "c", [{"up", 1}]}
      & {[], 2, "b", []}
      & {[], 1, "a", []}
      & Empty
      '''

  """
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
