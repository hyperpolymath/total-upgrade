# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell <jonathan@hyperpolymath.io>

defmodule Mix.Tasks.Dnfinition.Placement do
  use Mix.Task

  @shortdoc "Run a placement decision dry-run and emit an audit event"

  @moduledoc """
  Usage:

      mix dnfinition.placement --input /path/to/input.json
      cat input.json | mix dnfinition.placement
      mix dnfinition.placement --package foo --intent cli_tool --candidates toolbox,nerdctl
      mix dnfinition.placement --package foo --intent gui_app --linux-auto --validate --validate-output
      mix dnfinition.placement --package foo --intent gui_app --bsd-auto --validate --validate-output
      mix dnfinition.placement --package foo --intent gui_app --unix-auto --validate --validate-output
      mix dnfinition.placement --package foo --intent gui_app --kinoite-auto --validate --validate-output
  """

  alias DNFinition.Placement.BsdCollector
  alias DNFinition.Placement.Engine
  alias DNFinition.Placement.KinoiteCollector
  alias DNFinition.Placement.LinuxCollector
  alias DNFinition.Placement.UnixCollector

  @impl true
  def run(args) do
    {opts, _rest, _} =
      OptionParser.parse(args,
        switches: [
          input: :string,
          package: :string,
          intent: :string,
          candidates: :string,
          profile: :string,
          dry_run: :boolean,
          kinoite_auto: :boolean,
          linux_auto: :boolean,
          bsd_auto: :boolean,
          unix_auto: :boolean,
          validate: :boolean,
          validate_output: :boolean
        ]
      )

    input =
      case opts[:input] do
        nil -> read_stdin_or_flags(opts)
        path -> File.read!(path) |> Jason.decode!()
      end

    input =
      input
      |> Map.put_new("dry_run", Keyword.get(opts, :dry_run, true))
      |> maybe_override("profile", opts[:profile])
      |> maybe_apply_kinoite(opts[:kinoite_auto])
      |> maybe_apply_linux(opts[:linux_auto])
      |> maybe_apply_bsd(opts[:bsd_auto])
      |> maybe_apply_unix(opts[:unix_auto])

    input = maybe_validate_input(input, opts[:validate])

    {:ok, decision} = Engine.decide(input)
    maybe_validate_output(decision, opts[:validate_output])
    IO.puts(Jason.encode!(decision, pretty: true))
  end

  defp read_stdin_or_flags(opts) do
    if flags_present?(opts) do
      from_flags(opts)
    else
      case IO.read(:stdio, :eof) do
        "" -> from_flags(opts)
        data -> Jason.decode!(data)
      end
    end
  end

  defp from_flags(opts) do
    %{
      "package_id" => opts[:package] || "unknown",
      "intent" => opts[:intent] || "other",
      "candidates" => candidates_from_flag(opts[:candidates]),
      "scores" => %{},
      "dry_run" => Keyword.get(opts, :dry_run, true)
    }
  end

  defp candidates_from_flag(nil), do: []

  defp candidates_from_flag(value) do
    value
    |> String.split(",", trim: true)
  end

  defp maybe_override(input, _key, nil), do: input

  defp maybe_override(input, key, value) do
    Map.put(input, key, value)
  end

  defp flags_present?(opts) do
    Enum.any?([:package, :intent, :candidates, :profile], fn key ->
      Keyword.has_key?(opts, key)
    end)
  end

  defp maybe_apply_kinoite(input, true) do
    intent = Map.get(input, "intent", "other")
    auto = KinoiteCollector.collect(intent)
    candidates = Map.get(input, "candidates", [])

    input
    |> maybe_set_candidates(candidates, auto["candidates"])
    |> Map.update("scores", auto["scores"], &Map.merge(auto["scores"], &1))
  end

  defp maybe_apply_kinoite(input, _), do: input

  defp maybe_apply_linux(input, true) do
    intent = Map.get(input, "intent", "other")
    auto = LinuxCollector.collect(intent)
    candidates = Map.get(input, "candidates", [])

    input
    |> maybe_set_candidates(candidates, auto["candidates"])
    |> Map.update("scores", auto["scores"], &Map.merge(auto["scores"], &1))
    |> Map.put_new("environment", auto["environment"])
  end

  defp maybe_apply_linux(input, _), do: input

  defp maybe_apply_bsd(input, true) do
    intent = Map.get(input, "intent", "other")
    auto = BsdCollector.collect(intent)
    candidates = Map.get(input, "candidates", [])

    input
    |> maybe_set_candidates(candidates, auto["candidates"])
    |> Map.update("scores", auto["scores"], &Map.merge(auto["scores"], &1))
    |> Map.put_new("environment", auto["environment"])
  end

  defp maybe_apply_bsd(input, _), do: input

  defp maybe_apply_unix(input, true) do
    intent = Map.get(input, "intent", "other")
    auto = UnixCollector.collect(intent)
    candidates = Map.get(input, "candidates", [])

    input
    |> maybe_set_candidates(candidates, auto["candidates"])
    |> Map.update("scores", auto["scores"], &Map.merge(auto["scores"], &1))
    |> Map.put_new("environment", auto["environment"])
  end

  defp maybe_apply_unix(input, _), do: input

  defp maybe_set_candidates(input, [], auto_candidates) do
    Map.put(input, "candidates", auto_candidates)
  end

  defp maybe_set_candidates(input, _candidates, _auto_candidates), do: input

  defp maybe_validate_input(input, true) do
    validate_schema(schema_path("placement_input.schema.json"), input)
    input
  end

  defp maybe_validate_input(input, _), do: input

  defp maybe_validate_output(decision, true) do
    validate_schema(schema_path("placement_decision.schema.json"), decision)
  end

  defp maybe_validate_output(_decision, _), do: :ok

  defp validate_schema(schema_path, data) do
    schema =
      schema_path
      |> File.read!()
      |> Jason.decode!()
      |> ExJsonSchema.Schema.resolve()

    case ExJsonSchema.Validator.validate(schema, data) do
      :ok -> :ok
      {:error, errors} -> raise "Schema validation failed: #{inspect(errors)}"
    end
  end

  defp schema_path(filename) do
    Path.expand("../../../../../docs/#{filename}", __DIR__)
  end
end
