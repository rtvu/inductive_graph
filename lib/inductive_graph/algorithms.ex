defmodule InductiveGraph.Algorithms do

  def dfs(g, v), do: dfs(g, [v], [])

  defp dfs(_g, [], r), do: Enum.reverse(r)
  defp dfs(g, [v | vs], r) do
    if not InductiveGraph.empty?(g) do
      case InductiveGraph.decompose(g, v) do
        {:ok, c, g} ->
          vs = successors(c) ++ vs
          r = [v | r]
          dfs(g, vs, r)
        :error ->
          dfs(g, vs, r)
      end
    else
      Enum.reverse(r)
    end
  end

  def bfs(g, v), do: bfs(g, :queue.from_list([v]), [])

  defp bfs(g, q, r) do
    if :queue.is_empty(q) or InductiveGraph.empty?(g) do
      Enum.reverse(r)
    else
      {{:value, v}, q} = :queue.out(q)
      case InductiveGraph.decompose(g, v) do
        {:ok, c, g} ->
          q = :queue.join(q, :queue.from_list(successors(c)))
          r = [v | r]
          bfs(g, q, r)
        :error ->
          bfs(g, q, r)
      end
    end

  end


  defp successors({_p, v, _vv, s}) do
    s |> Enum.map(fn {_, n} -> n end) |> List.delete(v)
  end



  # defp
end
