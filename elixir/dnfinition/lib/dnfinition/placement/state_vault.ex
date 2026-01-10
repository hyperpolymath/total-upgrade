# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell <jonathan@hyperpolymath.io>

defmodule DNFinition.Placement.StateVault do
  @moduledoc """
  Centralize retained state for "state_vault" profile.
  """

  require Logger

  def capture(operation_id, package_id, paths, opts \\ []) do
    vault_path = Keyword.fetch!(opts, :vault_path)
    dry_run = Keyword.get(opts, :dry_run, true)

    entry_dir = Path.join(vault_path, "#{operation_id}_#{safe_name(package_id)}")
    File.mkdir_p!(entry_dir)

    manifest =
      paths
      |> Enum.map(&capture_path(&1, entry_dir, dry_run))
      |> Enum.reject(&is_nil/1)

    manifest_path = Path.join(entry_dir, "manifest.json")
    File.write!(manifest_path, Jason.encode!(manifest, pretty: true))

    :telemetry.execute(
      [:dnfinition, :placement, :state_vault],
      %{count: 1, paths: length(paths)},
      %{
        operation_id: operation_id,
        package_id: package_id,
        vault_path: vault_path,
        entry_dir: entry_dir,
        dry_run: dry_run
      }
    )

    {:ok, %{entry_dir: entry_dir, manifest: manifest}}
  end

  defp capture_path(path, entry_dir, dry_run) do
    if File.exists?(path) do
      dest = Path.join(entry_dir, Path.basename(path))

      if dry_run do
        %{source: path, dest: dest, copied: false}
      else
        File.cp_r!(path, dest)
        %{source: path, dest: dest, copied: true}
      end
    else
      Logger.warning("State vault path missing: #{path}")
      nil
    end
  end

  defp safe_name(name) do
    name
    |> String.replace("/", "_")
    |> String.replace(":", "_")
  end
end
