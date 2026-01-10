# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell <jonathan@hyperpolymath.io>

defmodule DNFinition.Placement.UnmanagedDetector do
  @moduledoc """
  Read-only detection of software not managed by a known package manager.
  """

  import Bitwise

  @default_paths [
    "/opt",
    "/usr/local/bin",
    "/usr/local/sbin",
    "/usr/local/share",
    ".local/bin",
    ".local/share",
    "Applications"
  ]

  def detect(opts \\ []) do
    paths = Keyword.get(opts, :paths, [])
    roots = if paths == [], do: default_paths(), else: paths

    roots
    |> Enum.flat_map(&scan_root/1)
    |> Enum.uniq_by(& &1.path)
  end

  defp default_paths do
    home = System.user_home!()

    Enum.map(@default_paths, fn path ->
      if String.starts_with?(path, "/") do
        path
      else
        Path.join(home, path)
      end
    end)
  end

  defp scan_root(root) do
    if File.dir?(root) do
      case File.ls(root) do
        {:ok, entries} ->
          entries
          |> Enum.map(&Path.join(root, &1))
          |> Enum.flat_map(&classify_path/1)

        _ ->
          []
      end
    else
      []
    end
  end

  defp classify_path(path) do
    cond do
      String.ends_with?(path, ".AppImage") -> [entry(path, "appimage", "replace_binary")]
      File.dir?(path) and String.ends_with?(path, ".app") ->
        [entry(path, "app_bundle", "replace_bundle")]

      File.dir?(path) and File.exists?(Path.join(path, "updater")) ->
        [entry(path, "vendor_updater", "vendor_tool")]

      File.regular?(path) and executable?(path) ->
        [entry(path, "binary", "unknown")]

      true ->
        []
    end
  end

  defp entry(path, kind, upgrade_route) do
    info = file_info(path)

    %{
      path: path,
      name: Path.basename(path),
      kind: kind,
      upgrade_route: upgrade_route,
      symlink: info.symlink,
      target: info.target
    }
  end

  defp executable?(path) do
    case File.stat(path) do
      {:ok, %{mode: mode}} -> (mode &&& 0o111) != 0
      _ -> false
    end
  end

  defp file_info(path) do
    case File.lstat(path) do
      {:ok, %{type: :symlink}} ->
        target =
          case File.read_link(path) do
            {:ok, link} -> Path.expand(link, Path.dirname(path))
            _ -> nil
          end

        %{symlink: true, target: target}

      _ ->
        %{symlink: false, target: nil}
    end
  end
end
