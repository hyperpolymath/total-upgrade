# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell <jonathan@hyperpolymath.io>

defmodule Mix.Tasks.Dnfinition.UnmanagedScan do
  use Mix.Task

  @shortdoc "Scan for unmanaged installs and emit audit events"

  @moduledoc """
  Usage:

      mix dnfinition.unmanaged_scan
      mix dnfinition.unmanaged_scan --paths /opt,/usr/local/bin
  """

  alias DNFinition.Audit
  alias DNFinition.Placement.UnmanagedAdvisor
  alias DNFinition.Placement.UnmanagedDetector

  @impl true
  def run(args) do
    {opts, _rest, _} =
      OptionParser.parse(args,
        switches: [
          paths: :string,
          suggest: :boolean
        ]
      )

    paths = parse_paths(opts[:paths])
    entries = UnmanagedDetector.detect(paths: paths)
    Audit.emit_unmanaged_detection(%{entries: entries, paths: paths})

    if opts[:suggest] do
      suggestions = UnmanagedAdvisor.suggest(entries)
      Audit.emit_unmanaged_suggestion(%{entries: suggestions, paths: paths})
      IO.puts(Jason.encode!(suggestions, pretty: true))
    else
      IO.puts(Jason.encode!(entries, pretty: true))
    end
  end

  defp parse_paths(nil), do: []

  defp parse_paths(value) do
    value
    |> String.split(",", trim: true)
  end
end
