# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell <jonathan@hyperpolymath.io>

defmodule DNFinition.Placement.LogScanner do
  @moduledoc """
  Read-only scan of core OS logs for missed or failed updates.
  """

  @core_logs [
    "/var/log/messages",
    "/var/log/syslog",
    "/var/log/dnf.log",
    "/var/log/apt/history.log",
    "/var/log/pacman.log",
    "/var/log/zypper.log"
  ]

  @patterns %{
    update_failed: ~r/(failed|error|err:|fatal)/i,
    update_skipped: ~r/(skipped|held back|kept back|not upgraded)/i,
    security: ~r/(security|vulnerability|cve-\d{4}-\d+)/i
  }

  def scan(opts \\ []) do
    limit = Keyword.get(opts, :limit, 500)
    since = Keyword.get(opts, :since, "7 days ago")

    file_findings =
      @core_logs
      |> Enum.flat_map(&scan_file(&1, limit))

    journal_findings = scan_journal(since, limit)

    (file_findings ++ journal_findings)
    |> Enum.take(limit)
  end

  defp scan_file(path, limit) do
    if File.regular?(path) do
      path
      |> tail_lines(limit)
      |> Enum.flat_map(&classify_line(path, &1))
    else
      []
    end
  end

  defp scan_journal(since, limit) do
    case System.find_executable("journalctl") do
      nil ->
        []

      _ ->
        args = ["--since", since, "--no-pager", "-n", Integer.to_string(limit)]

        case System.cmd("journalctl", args, stderr_to_stdout: true) do
          {output, 0} ->
            output
            |> String.split("\n", trim: true)
            |> Enum.flat_map(&classify_line("journalctl", &1))

          _ ->
            []
        end
    end
  end

  defp classify_line(source, line) do
    @patterns
    |> Enum.filter(fn {_key, regex} -> Regex.match?(regex, line) end)
    |> Enum.map(fn {key, _} ->
      %{
        source: source,
        category: Atom.to_string(key),
        line: line
      }
    end)
  end

  defp tail_lines(path, limit) do
    path
    |> File.stream!()
    |> Enum.reduce(:queue.new(), fn line, queue ->
      queue = :queue.in(line, queue)
      if :queue.len(queue) > limit, do: :queue.drop(queue), else: queue
    end)
    |> :queue.to_list()
  end
end
