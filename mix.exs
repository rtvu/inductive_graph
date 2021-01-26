defmodule InductiveGraph.MixProject do
  use Mix.Project

  def project() do
    [
      app: :inductive_graph,
      version: "0.1.1",
      elixir: "~> 1.11",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env() == :prod,
      description: description(),
      package: package(),
      deps: deps(),
      name: "InductiveGraph",
      source_url: "https://github.com/rtvu/inductive_graph",
      docs: [
        main: "readme",
        extras: ["README.md"],
        groups_for_functions: [
          {:"Construction Functions", & &1[:construction] == true},
          {:"Destruction Functions", & &1[:destruction] == true},
          {:"Update Functions", & &1[:update] == true},
          {:"Conversion Functions", & &1[:conversion] == true},
          {:"Inspection Functions", & &1[:inspection] == true}
        ]
      ]
    ]
  end

  def application() do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps() do
    [
      {:ex_doc, "~> 0.23", only: :dev, runtime: false}
    ]
  end

  defp description() do
    "Inductive graph library implemented in Elixir."
  end

  defp package() do
    [
      licenses: ["MIT License"],
      maintainers: [],
      links: %{"Github" => "https://github.com/rtvu/inductive_graph"}
    ]
  end
end
