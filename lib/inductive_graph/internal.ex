defmodule InductiveGraph.Internal do
  @moduledoc """
  Functions to manage internal representation of inductive graph.
  """

  alias InductiveGraph.Utilities

  @type vertex :: InductiveGraph.vertex
  @type value :: InductiveGraph.value
  @type tagged_vertex :: InductiveGraph.tagged_vertex
  @type tagged_edge :: InductiveGraph.tagged_edge
  @type adjacents :: %{required(vertex) => [value]}
  @type context :: {predecessors :: adjacents, value, successors :: adjacents}
  @type t :: %{required(vertex) => context}

  @doc """
  Creates an empty graph.
  """
  @spec empty_graph() :: t
  def empty_graph(), do: %{}

  @doc """
  Determines if `graph` is empty.
  """
  @spec empty?(t) :: boolean
  def empty?(graph)
  def empty?(graph = %{}), do: graph == %{}

  @doc """
  Updates `context` by `function` based on `update_type`.
  """
  @spec update_context(context, :context | :predecessors | :value | :successors, (context -> context) | (adjacents -> adjacents) | (value -> value)) :: context
  def update_context(context = {predecessors, value, successors}, update_type, function) do
    case update_type do
      :context ->
        function.(context)
      :value ->
        {predecessors, function.(value), successors}
      :predecessors ->
        {function.(predecessors), value, successors}
      :successors ->
        {predecessors, value, function.(successors)}
    end
  end

  @doc """
  Adds edges with `vertex` to `adjacents`.
  """
  @spec add_edges_to_adjacents(adjacents, vertex, [value]) :: adjacents
  def add_edges_to_adjacents(adjacents, vertex, values) do
    Map.update(adjacents, vertex, values, &Enum.concat(&1, values))
  end

  @doc """
  Adds edges with `vertex` to either predecessor or successor adjacents in
  `context`.
  """
  @spec add_edges_to_context(context, vertex, [value], :predecessors | :successors) :: context
  def add_edges_to_context(context, vertex, edge_values, position) do
    update_context(context, position, &add_edges_to_adjacents(&1, vertex, edge_values))
  end

  @doc """
  Inserts `edge` into `graph`.
  """
  @spec insert_edge(t, tagged_edge) :: {:ok, t} | :error
  def insert_edge(graph, edge)
  def insert_edge(graph, {from_vertex, to_vertex, value}) do
    with true <- Map.has_key?(graph, from_vertex),
         true <- Map.has_key?(graph, to_vertex),
         {:ok, graph} <- Utilities.map_update(graph, from_vertex, &add_edges_to_context(&1, to_vertex, [value], :successors)),
         {:ok, graph} <- Utilities.map_update(graph, to_vertex, &add_edges_to_context(&1, from_vertex, [value], :predecessors)) do
      {:ok, graph}
    else
      _error -> :error
    end
  end

  @doc """
  Inserts `edges` into `graph`.
  """
  @spec insert_edges(t, [tagged_edge]) :: {:ok, t} | :error
  def insert_edges(graph, edges) do
    insert =
      fn
        edge, {:ok, graph} -> insert_edge(graph, edge)
        _edge, :error -> :error
      end

    List.foldl(edges, {:ok, graph}, insert)
  end

  @doc """
  Creates a new context based on `value`.
  """
  @spec new_context(value) :: context
  def new_context(value), do: {%{}, value, %{}}

  @doc """
  Creates a graph from lists `vertices` and `edges`.
  """
  @spec make_graph([tagged_vertex], [tagged_edge]) :: {:ok, t} | :error
  def make_graph(vertices, edges) do
    convert = fn {vertex, value} -> {vertex, new_context(value)} end

    vertices
    |> Stream.map(convert)
    |> Map.new()
    |> insert_edges(edges)
  end

  @doc """
  Removes edges with `vertex` from either predecessor or successor adjacents in
  `context`.
  """
  @spec remove_edges_from_context(context, vertex, :predecessors | :successors) :: context
  def remove_edges_from_context(context, vertex, position) do
    update = &Map.delete(&1, vertex)
    update_context(context, position, update)
  end

  @doc """
  Removes edges with `edge_vertex` from either predecessor or successor
  adjacents in contexts of `vertices`.
  """
  @spec prune_adjacents(t, [vertex], vertex, :predecessors | :successors) :: {:ok, t} | :error
  def prune_adjacents(graph, vertices, edge_vertex, position) do
    remove =
      fn
        vertex, {:ok, graph} -> Utilities.map_update(graph, vertex, &remove_edges_from_context(&1, edge_vertex, position))
        _vertex, :error -> :error
      end

    List.foldl(vertices, {:ok, graph}, remove)
  end

  @doc """
  Converts `adjacents` to `[{value, vertex}]`.
  """
  @spec from_adjacents(adjacents) :: InductiveGraph.adjacents
  def from_adjacents(adjacents) do
    adjacents
    |> Map.to_list()
    |> Enum.flat_map(fn {key, values} -> Enum.map(values, &({&1, key})) end)
  end

  @doc """
  Converts `[{value, vertex}]` to `adjacents`.
  """
  @spec to_adjacents(InductiveGraph.adjacents) :: adjacents
  def to_adjacents(adjacents) do
    convert = fn {value, vertex}, adjacents -> add_edges_to_adjacents(adjacents, vertex, [value]) end
    List.foldl(adjacents, %{}, convert)
  end

  @doc """
  Converts `{[{value, vertex}], vertex, value, [{value, vertex}]}` to context.
  """
  @spec to_context(InductiveGraph.context) :: context
  def to_context(context)
  def to_context({predecessors, _vertex, value, successors}) do
    {to_adjacents(predecessors), value, to_adjacents(successors)}
  end

  @doc """
  Converts `context` to `{[{value, vertex}], vertex, value, [{value, vertex}]}`.
  """
  @spec from_context(context, vertex) :: InductiveGraph.context
  def from_context(context, vertex)
  def from_context({predecessors, value, successors}, vertex) do
    {from_adjacents(predecessors), vertex, value, from_adjacents(successors)}
  end

  @doc """
  Decomposes `graph` into the context containing `vertex` and the remaining
  graph.
  """
  @spec decompose(t, vertex) :: {:ok, InductiveGraph.context, t} | :error
  def decompose(graph, vertex) do
    with {:ok, {predecessors, value, successors}} <- Map.fetch(graph, vertex),
         graph = Map.delete(graph, vertex),
         vertex_removed_predecessors = Map.delete(predecessors, vertex),
         {:ok, graph} <- prune_adjacents(graph, Map.keys(vertex_removed_predecessors), vertex, :successors),
         vertex_removed_successors = Map.delete(successors, vertex),
         {:ok, graph} <- prune_adjacents(graph, Map.keys(vertex_removed_successors), vertex, :predecessors) do
      predecessors = from_adjacents(vertex_removed_predecessors)
      successors = from_adjacents(successors)
      {:ok, {predecessors, vertex, value, successors}, graph}
    else
      _error -> :error
    end
  end

  @doc """
  Lists all vertices in `graph`.
  """
  @spec list_vertices(t) :: [tagged_vertex]
  def list_vertices(graph) do
    format = fn {vertex, {_predecessors, value, _successors}} -> {vertex, value} end

    graph
    |> Map.to_list()
    |> Enum.map(format)
  end

  @doc """
  Counts number of vertices in `graph`.
  """
  @spec count_vertices(t) :: non_neg_integer
  def count_vertices(graph) do
    map_size(graph)
  end

  @doc """
  Gets range of vertex values in `graph`.

  Returns `{:ok, minimum, maximum}` for graphs with at least one vertex. Returns
  `:error` for empty graph.
  """
  @spec vertex_range(t) :: {:ok, min :: integer, max :: integer} | :error
  def vertex_range(graph) do
    case Map.keys(graph) do
      [] ->
        :error
      [vertex | vertices] ->
        min_max = fn vertex, {minimum, maximum} -> {min(minimum, vertex), max(maximum, vertex)} end
        {minimum, maximum} = List.foldl(vertices, {vertex, vertex}, min_max)
        {:ok, minimum, maximum}
    end
  end

  @doc """
  Inserts `vertices` into `graph`.
  """
  @spec insert_vertices(t, [tagged_vertex]) :: {:ok, t} | :error
  def insert_vertices(graph, vertices) do
    insert =
      fn
        vertex, {:ok, graph} -> insert_vertex(graph, vertex)
        _vertex, :error -> :error
      end

    List.foldl(vertices, {:ok, graph}, insert)
  end

  @doc """
  Inserts `vertex` into `graph`.
  """
  @spec insert_vertex(t, tagged_vertex) :: {:ok, t} | :error
  def insert_vertex(graph, vertex)
  def insert_vertex(graph, {vertex, value}) do
    case Map.has_key?(graph, vertex) do
      true -> :error
      false -> {:ok, Map.put(graph, vertex, new_context(value))}
    end
  end

  @doc """
  Lists all edges in `graph`.
  """
  @spec list_edges(t) :: [tagged_edge]
  def list_edges(graph) do
    for {from_vertex, {_predecessors, _label, successors}} <- Map.to_list(graph),
        {to_vertex, values} <- Map.to_list(successors),
        value <- values do
      {from_vertex, to_vertex, value}
    end
  end

  @doc """
  Pretty prints inductive representation of `graph`.

  If `count` is provided, then up to `count` number of contexts will be shown.
  """
  @spec pretty_print(t, integer) :: String.t
  def pretty_print(graph, count \\ -1) do
    vertices = graph |> Map.keys() |> Enum.sort() |> Enum.reverse()
    pretty_print(graph, vertices, count, "| ")
  end

  # Pretty prints inductive representation of `graph`.
  @spec pretty_print(t, [vertex], integer, String.t) :: String.t
  defp pretty_print(graph, vertices, count, result)
  defp pretty_print(_graph, [], _count, result), do: result <> "Empty"
  defp pretty_print(_graph, _vertices, 0, result), do: result <> "InductiveGraph"
  defp pretty_print(graph, [vertex | vertices], count, result) do
    {:ok, context, graph} = decompose(graph, vertex)
    result = result <> inspect(context) <> "\n& "
    pretty_print(graph, vertices, count - 1, result)
  end

  @doc """
  Merges `context` into `graph`.
  """
  @spec merge(t, InductiveGraph.context) :: {:ok, t} | :error
  def merge(graph, context)
  def merge(graph, {predecessors, vertex, value, successors}) do
    flip =
      fn
        :predecessors -> :successors
        :successors -> :predecessors
      end

    process =
      fn
        {value, adjacent_vertex}, {:ok, graph, context}, position ->
          with {:ok, graph} <- Utilities.map_update(graph, adjacent_vertex, &add_edges_to_context(&1, vertex, [value], flip.(position))) do
            context = add_edges_to_context(context, adjacent_vertex, [value], position)
            {:ok, graph, context}
          else
            _error -> :error
          end
        _adjacent, :error, _position ->
          :error
      end

    with false <- Map.has_key?(graph, vertex),
         context = new_context(value),
         {:ok, graph, context} <- List.foldl(predecessors, {:ok, graph, context}, &process.(&1, &2, :predecessors)),
         {:ok, graph, context} <- List.foldl(successors, {:ok, graph, context}, &process.(&1, &2, :successors)) do
      {:ok, Map.put(graph, vertex, context)}
    else
      _error -> :error
    end
  end
end
