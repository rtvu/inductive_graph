defmodule InductiveGraph.MixProject do
  use Mix.Project

  def project do
    [
      app: :inductive_graph,
      version: "0.1.0-dev",
      elixir: "~> 1.9",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      docs: [
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

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:ex_doc, "~> 0.21", only: :dev, runtime: false}
    ]
  end
end
