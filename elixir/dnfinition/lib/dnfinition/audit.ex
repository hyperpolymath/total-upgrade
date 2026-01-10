# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell <jonathan@hyperpolymath.io>

defmodule DNFinition.Audit do
  @moduledoc """
  Append-only audit emitter for structured events.
  """

  require Logger

  @default_log_dir "/var/lib/dnfinition/audit"
  @default_feedback_outbox Path.join([
                            System.user_home!(),
                            ".local",
                            "state",
                            "feedback-a-tron",
                            "inbox",
                            "ambientops.jsonl"
                          ])

  def emit(event_type, payload, opts \\ []) do
    log_path = Keyword.get(opts, :log_path, audit_log_path())
    log_path = ensure_log_path(log_path)
    event = build_event(event_type, payload)
    encoded = Jason.encode!(event)

    File.write!(log_path, encoded <> "\n", [:append])
    forward_to_feedback_outbox(event)

    :ok
  end

  def emit_placement_decision(payload, opts \\ []) do
    emit("placement_decision", payload, opts)
  end

  def emit_unmanaged_detection(payload, opts \\ []) do
    emit("unmanaged_detection", payload, opts)
  end

  def emit_unmanaged_suggestion(payload, opts \\ []) do
    emit("unmanaged_suggestion", payload, opts)
  end

  def emit_log_scan(payload, opts \\ []) do
    emit("log_scan", payload, opts)
  end

  def audit_log_path do
    log_dir = Application.get_env(:dnfinition, :audit_log_dir, @default_log_dir)
    Path.join(log_dir, "audit.jsonl")
  end

  def feedback_outbox_path do
    Application.get_env(:dnfinition, :feedback_outbox_path, @default_feedback_outbox)
  end

  defp build_event(event_type, payload) do
    %{
      "event_type" => event_type,
      "timestamp" => DateTime.utc_now() |> DateTime.to_iso8601(),
      "host_id" => host_id(),
      "payload" => payload
    }
  end

  defp host_id do
    case :inet.gethostname() do
      {:ok, hostname} -> List.to_string(hostname)
      _ -> "unknown-host"
    end
  end

  defp ensure_log_path(path) do
    case File.mkdir_p(Path.dirname(path)) do
      :ok -> path
      {:error, reason} ->
        Logger.warning("Audit log dir unavailable (#{inspect(reason)}), using user fallback")
        fallback_log_path()
    end
  end

  defp fallback_log_path do
    home = System.user_home!()
    path = Path.join([home, ".local", "state", "dnfinition", "audit", "audit.jsonl"])
    File.mkdir_p(Path.dirname(path))
    path
  end

  defp forward_to_feedback_outbox(event) do
    path = feedback_outbox_path()
    File.mkdir_p(Path.dirname(path))
    File.write!(path, Jason.encode!(event) <> "\n", [:append])
  rescue
    _ -> Logger.warning("Failed to write feedback outbox event")
  end
end
