defmodule InductiveGraph.Internal do
  @moduledoc """
  Functions to manage internal representation of inductive graph.
  """

  alias InductiveGraph.Utilities

  @type value :: InductiveGraph.value
  @type edge_value :: InductiveGraph.edge_value
  @type vertex_value :: InductiveGraph.vertex_value
  @type vertex :: InductiveGraph.vertex
  @type neighbor :: InductiveGraph.neighbor
  @type tagged_vertex :: InductiveGraph.tagged_vertex
  @type tagged_edge :: InductiveGraph.tagged_edge
  @type adjacents :: %{required(neighbor) => [edge_value]}
  @type predecessors :: adjacents
  @type successors :: adjacents
  @type context :: {predecessors, vertex_value, successors}
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
  @spec update_context(context, :context | :predecessors | :vertex_value | :successors, (context -> context) | (adjacents -> adjacents) | (value -> value)) :: context
  def update_context(context = {predecessors, vertex_value, successors}, update_type, function) do
    case update_type do
      :context ->
        function.(context)
      :vertex_value ->
        {predecessors, function.(vertex_value), successors}
      :predecessors ->
        {function.(predecessors), vertex_value, successors}
      :successors ->
        {predecessors, vertex_value, function.(successors)}
    end
  end

  @doc """
  Adds `edge_values` with `neighbor` to `adjacents`.
  """
  @spec add_edges_to_adjacents(adjacents, neighbor, [edge_value]) :: adjacents
  def add_edges_to_adjacents(adjacents, neighbor, edge_values) do
    Map.update(adjacents, neighbor, edge_values, &Enum.concat(&1, edge_values))
  end

  @doc """
  Adds `edge_values` with `neighbor` to either predecessor or successor
  adjacents `position` in `context`.
  """
  @spec add_edges_to_context(context, neighbor, [edge_value], :predecessors | :successors) :: context
  def add_edges_to_context(context, neighbor, edge_values, position) do
    update_context(context, position, &add_edges_to_adjacents(&1, neighbor, edge_values))
  end

  @doc """
  Inserts `tagged_edge` into `graph`.
  """
  @spec insert_tagged_edge(t, tagged_edge) :: {:ok, t} | :error
  def insert_tagged_edge(graph, tagged_edge)
  def insert_tagged_edge(graph, {from_vertex, to_vertex, edge_value}) do
    with true <- Map.has_key?(graph, from_vertex),
         true <- Map.has_key?(graph, to_vertex),
         {:ok, graph} <- Utilities.map_update(graph, from_vertex, &add_edges_to_context(&1, to_vertex, [edge_value], :successors)),
         {:ok, graph} <- Utilities.map_update(graph, to_vertex, &add_edges_to_context(&1, from_vertex, [edge_value], :predecessors)) do
      {:ok, graph}
    else
      _error -> :error
    end
  end

  @doc """
  Inserts `tagged_edges` into `graph`.
  """
  @spec insert_tagged_edges(t, [tagged_edge]) :: {:ok, t} | :error
  def insert_tagged_edges(graph, tagged_edges) do
    insert =
      fn
        tagged_edge, {:ok, graph} -> insert_tagged_edge(graph, tagged_edge)
        _edge, :error -> :error
      end

    List.foldl(tagged_edges, {:ok, graph}, insert)
  end

  @doc """
  Creates a new context based on `vertex_value`.
  """
  @spec new_context(vertex_value) :: context
  def new_context(vertex_value), do: {%{}, vertex_value, %{}}

  @doc """
  Creates a graph from `tagged_vertices` and `tagged_edges`.
  """
  @spec make_graph([tagged_vertex], [tagged_edge]) :: {:ok, t} | :error
  def make_graph(tagged_vertices, tagged_edges) do
    convert = fn {vertex, vertex_value} -> {vertex, new_context(vertex_value)} end

    tagged_vertices
    |> Stream.map(convert)
    |> Map.new()
    |> insert_tagged_edges(tagged_edges)
  end

  @doc """
  Removes edges with `neighbor` from either predecessor or successor
  adjacents `position` in `context`.
  """
  @spec remove_edges_from_context(context, neighbor, :predecessors | :successors) :: context
  def remove_edges_from_context(context, neighbor, position) do
    update = &Map.delete(&1, neighbor)
    update_context(context, position, update)
  end

  @doc """
  Removes edges with `neighbor` from either predecessor or successor adjacents
  `position` in contexts of `vertices`.
  """
  @spec prune_adjacents(t, [vertex], neighbor, :predecessors | :successors) :: {:ok, t} | :error
  def prune_adjacents(graph, vertices, neighbor, position) do
    remove =
      fn
        vertex, {:ok, graph} -> Utilities.map_update(graph, vertex, &remove_edges_from_context(&1, neighbor, position))
        _vertex, :error -> :error
      end

    List.foldl(vertices, {:ok, graph}, remove)
  end

  @doc """
  Converts `adjacents` to `[{edge_value, neighbor}]`.
  """
  @spec from_adjacents(adjacents) :: InductiveGraph.adjacents
  def from_adjacents(adjacents) do
    adjacents
    |> Map.to_list()
    |> Enum.flat_map(fn {neighbor, edge_values} -> Enum.map(edge_values, &({&1, neighbor})) end)
  end

  @doc """
  Converts `[{edge_value, neighbor}]` to `adjacents`.
  """
  @spec to_adjacents(InductiveGraph.adjacents) :: adjacents
  def to_adjacents(adjacents) do
    convert = fn {edge_value, neighbor}, adjacents -> add_edges_to_adjacents(adjacents, neighbor, [edge_value]) end
    List.foldl(adjacents, %{}, convert)
  end

  @doc """
  Converts `{[{edge_value, neighbor}], vertex, vertex_value, [{edge_value, neighbor}]}`
  to context.
  """
  @spec to_context(InductiveGraph.context) :: context
  def to_context(context)
  def to_context({predecessors, _vertex, vertex_value, successors}) do
    {to_adjacents(predecessors), vertex_value, to_adjacents(successors)}
  end

  @doc """
  Converts `context` to `{[{edge_value, neighbor}], vertex, vertex_value, [{edge_value, neighbor}]}`.
  """
  @spec from_context(context, vertex) :: InductiveGraph.context
  def from_context(context, vertex)
  def from_context({predecessors, vertex_value, successors}, vertex) do
    {from_adjacents(predecessors), vertex, vertex_value, from_adjacents(successors)}
  end

  @doc """
  Decomposes `graph` into the context containing `vertex` and the remaining
  graph.
  """
  @spec decompose(t, vertex) :: {:ok, InductiveGraph.context, t} | :error
  def decompose(graph, vertex) do
    with {:ok, {predecessors, vertex_value, successors}} <- Map.fetch(graph, vertex),
         graph = Map.delete(graph, vertex),
         vertex_removed_predecessors = Map.delete(predecessors, vertex),
         {:ok, graph} <- prune_adjacents(graph, Map.keys(vertex_removed_predecessors), vertex, :successors),
         vertex_removed_successors = Map.delete(successors, vertex),
         {:ok, graph} <- prune_adjacents(graph, Map.keys(vertex_removed_successors), vertex, :predecessors) do
      predecessors = from_adjacents(vertex_removed_predecessors)
      successors = from_adjacents(successors)
      {:ok, {predecessors, vertex, vertex_value, successors}, graph}
    else
      _error -> :error
    end
  end

  @doc """
  Lists all vertices in `graph`.
  """
  @spec list_tagged_vertices(t) :: [tagged_vertex]
  def list_tagged_vertices(graph) do
    format = fn {vertex, {_predecessors, vertex_value, _successors}} -> {vertex, vertex_value} end

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
  Gets range of vertex in `graph`.

  Returns `{:ok, minimum, maximum}` for graphs with at least one vertex. Returns
  `:error` for empty graph.
  """
  @spec vertex_range(t) :: {:ok, minimum :: integer, maximum :: integer} | :error
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
  Inserts `tagged_vertices` into `graph`.
  """
  @spec insert_tagged_vertices(t, [tagged_vertex]) :: {:ok, t} | :error
  def insert_tagged_vertices(graph, tagged_vertices) do
    insert =
      fn
        tagged_vertex, {:ok, graph} -> insert_tagged_vertex(graph, tagged_vertex)
        _vertex, :error -> :error
      end

    List.foldl(tagged_vertices, {:ok, graph}, insert)
  end

  @doc """
  Inserts `tagged_vertex` into `graph`.
  """
  @spec insert_tagged_vertex(t, tagged_vertex) :: {:ok, t} | :error
  def insert_tagged_vertex(graph, tagged_vertex)
  def insert_tagged_vertex(graph, {tagged_vertex, vertex_value}) do
    case Map.has_key?(graph, tagged_vertex) do
      true -> :error
      false -> {:ok, Map.put(graph, tagged_vertex, new_context(vertex_value))}
    end
  end

  @doc """
  Lists all tagged edges in `graph`.
  """
  @spec list_tagged_edges(t) :: [tagged_edge]
  def list_tagged_edges(graph) do
    for {from_vertex, {_predecessors, _vertex_value, successors}} <- Map.to_list(graph),
        {to_vertex, edge_values} <- Map.to_list(successors),
        edge_value <- edge_values do
      {from_vertex, to_vertex, edge_value}
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
  def merge(graph, {predecessors, vertex, vertex_value, successors}) do
    process =
      fn
        {vertex_value, neighbor}, {:ok, graph, context}, position ->
          with {:ok, graph} <- Utilities.map_update(graph, neighbor, &add_edges_to_context(&1, vertex, [vertex_value], flip_adjacents_type(position))) do
            context = add_edges_to_context(context, neighbor, [vertex_value], position)
            {:ok, graph, context}
          else
            _error -> :error
          end
        _adjacent, :error, _position ->
          :error
      end

    with false <- Map.has_key?(graph, vertex),
         context = new_context(vertex_value),
         {:ok, graph, context} <- List.foldl(predecessors, {:ok, graph, context}, &process.(&1, &2, :predecessors)),
         {:ok, graph, context} <- List.foldl(successors, {:ok, graph, context}, &process.(&1, &2, :successors)) do
      {:ok, Map.put(graph, vertex, context)}
    else
      _error -> :error
    end
  end

  # Flips the adjacents type.
  @spec flip_adjacents_type(:predecessors) :: :successors
  @spec flip_adjacents_type(:successors) :: :predecessors
  defp flip_adjacents_type(:predecessors), do: :successors
  defp flip_adjacents_type(:successors), do: :predecessors

  @doc """
  Applies `function` to `adjacents`.
  """
  @spec map_adjacents(adjacents, function) :: adjacents
  def map_adjacents(adjacents, function) do
    Enum.into(adjacents, %{}, fn {neighbor, edge_values} -> {neighbor, Enum.map(edge_values, function)} end)
  end

  @doc """
  Applies `function` to every context in `graph`.
  """
  @spec map_graph(t, (InductiveGraph.context -> InductiveGraph.context)) :: t
  def map_graph(graph, function) do
    transform =
      fn
        {vertex, context} ->
          context = context |> from_context(vertex) |> function.() |> to_context()
          {vertex, context}
      end

    Enum.into(graph, %{}, transform)
  end

  @doc """
  Applies `function` to every vertex value in `graph`.
  """
  @spec map_vertices(t, (vertex_value -> vertex_value)) :: t
  def map_vertices(graph, function) do
    transform =
      fn
        {vertex, context} ->
          {vertex, update_context(context, :vertex_value, function)}
      end

    Enum.into(graph, %{}, transform)
  end

  @doc """
  Applies `function` to every edge value in `graph`.
  """
  @spec map_edges(t, (edge_value -> edge_value)) :: t
  def map_edges(graph, function) do
    transform =
      fn
        {vertex, context} ->
          context =
            context
            |> update_context(:predecessors, &map_adjacents(&1, function))
            |> update_context(:successors, &map_adjacents(&1, function))
          {vertex, context}
      end

    Enum.into(graph, %{}, transform)
  end

  @doc """
  Applies `vertex_function` to every vertex value and `edge_function` to every
  edge value in `graph`.
  """
  @spec map_vertices_and_edges(t, (vertex_value -> vertex_value), (edge_value -> edge_value)) :: t
  def map_vertices_and_edges(graph, vertex_function, edge_function) do
    transform =
      fn
        {vertex, context} ->
          context =
            context
            |> update_context(:predecessors, &map_adjacents(&1, edge_function))
            |> update_context(:successors, &map_adjacents(&1, edge_function))
            |> update_context(:vertex_value, vertex_function)
          {vertex, context}
      end

    Enum.into(graph, %{}, transform)
  end

  @doc """
  Determines if `vertex` is in `graph`.
  """
  @spec has_vertex?(t, vertex) :: boolean
  def has_vertex?(graph, vertex), do: Map.has_key?(graph, vertex)
end
