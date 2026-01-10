# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell <jonathan@hyperpolymath.io>

defmodule Mix.Tasks.Dnfinition.LogScan do
  use Mix.Task

  @shortdoc "Scan core OS logs for missed/failed updates"

  @moduledoc """
  Usage:

      mix dnfinition.log_scan
      mix dnfinition.log_scan --since "30 days ago" --limit 1000
  """

  alias DNFinition.Audit
  alias DNFinition.Placement.LogScanner

  @impl true
  def run(args) do
    {opts, _rest, _} =
      OptionParser.parse(args,
        switches: [
          since: :string,
          limit: :integer
        ]
      )

    findings = LogScanner.scan(since: opts[:since], limit: opts[:limit])
    Audit.emit_log_scan(%{findings: findings, since: opts[:since], limit: opts[:limit]})
    IO.puts(Jason.encode!(findings, pretty: true))
  end
end
