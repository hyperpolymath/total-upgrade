# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell <jonathan@hyperpolymath.io>

defmodule DNFinition.Placement.UnmanagedAdvisor do
  @moduledoc """
  Provide guided upgrade suggestions for unmanaged installs.
  """

  def suggest(entries, opts \\ []) do
    policy = Keyword.get(opts, :policy, %{})

    Enum.map(entries, fn entry ->
      {origin, confidence, evidence} = infer_origin(entry)

      entry
      |> Map.put(:origin, origin)
      |> Map.put(:origin_confidence, confidence)
      |> Map.put(:origin_evidence, evidence)
      |> Map.put(:suggested_surface, suggested_surface(entry, origin, policy))
      |> Map.put(:suggested_route, suggested_route(entry, origin))
    end)
  end

  defp suggested_surface(%{kind: "appimage"}, _origin, _policy), do: "appimage"
  defp suggested_surface(%{kind: "app_bundle"}, _origin, _policy), do: "app_bundle"
  defp suggested_surface(%{kind: "vendor_updater"}, _origin, _policy), do: "vendor_tool"
  defp suggested_surface(_entry, "asdf", _policy), do: "asdf"
  defp suggested_surface(_entry, "pipx", _policy), do: "pipx"
  defp suggested_surface(_entry, "pip_user", _policy), do: "pip"
  defp suggested_surface(_entry, "cargo", _policy), do: "cargo"
  defp suggested_surface(_entry, "npm_global", _policy), do: "npm"
  defp suggested_surface(_entry, "go_install", _policy), do: "go"
  defp suggested_surface(_entry, "gem_user", _policy), do: "gem"
  defp suggested_surface(_entry, _origin, _policy), do: "unknown"

  defp suggested_route(%{upgrade_route: route}, _origin) when route != "unknown", do: route
  defp suggested_route(_entry, "asdf"), do: "asdf_update"
  defp suggested_route(_entry, "pipx"), do: "pipx_upgrade"
  defp suggested_route(_entry, "pip_user"), do: "pip_install_user_upgrade"
  defp suggested_route(_entry, "cargo"), do: "cargo_install_update"
  defp suggested_route(_entry, "npm_global"), do: "npm_global_update"
  defp suggested_route(_entry, "go_install"), do: "go_install_update"
  defp suggested_route(_entry, "gem_user"), do: "gem_update"
  defp suggested_route(_entry, _origin), do: "unknown"

  defp infer_origin(entry) do
    path = Map.get(entry, :path, "")
    target = Map.get(entry, :target)
    name = Map.get(entry, :name, "")
    resolved = target || path

    cond do
      String.contains?(resolved, "/.asdf/shims/") ->
        {"asdf", :high, ["path:asdf_shims"]}

      String.contains?(resolved, "/.cargo/bin/") ->
        {"cargo", :high, ["path:cargo_bin"]}

      String.contains?(resolved, "/.local/share/pipx/venvs/") ->
        {"pipx", :high, ["path:pipx_venv"]}

      String.contains?(resolved, "/go/bin/") ->
        {"go_install", :high, ["path:go_bin"]}

      String.contains?(resolved, "/.gem/") ->
        {"gem_user", :high, ["path:gem_dir"]}

      String.contains?(resolved, "/node_modules/.bin/") ->
        {"npm_global", :high, ["path:node_modules_bin"]}

      name == "pipx" ->
        {"pipx", :medium, ["name:pipx"]}

      python_script?(resolved) ->
        {"pip_user", :medium, ["shebang:python"]}

      node_script?(resolved) ->
        {"npm_global", :medium, ["shebang:node"]}

      true ->
        {"unknown", :low, []}
    end
  end

  defp python_script?(path) do
    shebang = read_shebang(path)
    String.contains?(shebang, "python")
  end

  defp node_script?(path) do
    shebang = read_shebang(path)
    String.contains?(shebang, "node")
  end

  defp read_shebang(path) do
    if File.regular?(path) do
      case File.read(path) do
        {:ok, content} ->
          content
          |> String.split("\n", parts: 2)
          |> List.first()
          |> to_string()

        _ ->
          ""
      end
    else
      ""
    end
  end
end
