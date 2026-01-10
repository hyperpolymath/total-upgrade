# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell <jonathan@hyperpolymath.io>

defmodule DNFinition.Placement.Engine do
  @moduledoc """
  Placement decision stub wired to audit emission.
  """

  alias DNFinition.Audit
  alias DNFinition.Placement.CandidateTagger
  alias DNFinition.Placement.ContainerImagePicker
  alias DNFinition.Placement.Policy
  alias DNFinition.Placement.StatePaths
  alias DNFinition.Placement.StateVault

  def decide(input, opts \\ []) do
    policy = Keyword.get(opts, :policy, Policy.load())
    decision = score_and_select(input, policy)

    Audit.emit_placement_decision(decision)
    emit_telemetry(decision)
    {:ok, decision}
  end

  defp score_and_select(input, policy) do
    intent = Map.get(input, "intent", "other")
    profile = Map.get(input, "profile", "default")
    candidates = Map.get(input, "candidates", [])
    candidate_meta = candidate_meta_with_images(candidates, Map.get(input, "candidate_meta", %{}))
    candidate_tags = Map.get(input, "candidate_tags")

    candidate_tags =
      if candidate_tags in [nil, %{}] do
        CandidateTagger.tag_candidates(candidates, candidate_meta)
      else
        candidate_tags
      end
    scores = Map.get(input, "scores", %{})
    constrained = apply_constraints(intent, candidates, policy.constraints)

    selected =
      case pick_best(constrained, scores, candidate_tags, policy, profile) do
        nil -> List.first(candidates)
        surface -> surface
      end

    %{
      "operation_id" => Map.get(input, "operation_id", default_operation_id()),
      "package_id" => Map.get(input, "package_id", "unknown"),
      "intent" => intent,
      "profile" => profile,
      "candidates" => candidates,
      "candidate_tags" => candidate_tags,
      "candidate_meta" => candidate_meta,
      "environment" => Map.get(input, "environment", %{}),
      "scores" => scores,
      "selected_surface" => selected,
      "constraints" => constraint_reasons(intent, policy.constraints),
      "user_override" => Map.get(input, "user_override", false),
      "override_reason" => Map.get(input, "override_reason", nil),
      "cost_risk" => Map.get(input, "cost_risk", "unknown"),
      "dry_run" => Map.get(input, "dry_run", true),
      "result" => Map.get(input, "result", "accepted")
    }
    |> maybe_capture_state(profile, policy, input)
  end

  defp apply_constraints(intent, candidates, constraints) do
    allowed = Map.get(constraints, intent, candidates)

    candidates
    |> Enum.filter(&(&1 in allowed))
    |> case do
      [] -> candidates
      filtered -> filtered
    end
  end

  defp constraint_reasons(intent, constraints) do
    case Map.get(constraints, intent) do
      nil -> []
      _ -> ["intent_prefers_#{intent}"]
    end
  end

  defp pick_best(candidates, scores, candidate_tags, policy, profile) do
    candidates
    |> Enum.map(fn surface ->
      {surface,
       weighted_total(
         Map.get(scores, surface, %{}),
         Map.get(candidate_tags, surface, []),
         policy,
         profile
       )}
    end)
    |> Enum.max_by(fn {_surface, total} -> total end, fn -> nil end)
    |> case do
      nil -> nil
      {surface, _} -> surface
    end
  end

  defp weighted_total(surface_scores, tags, policy, profile) do
    base =
      Enum.reduce(surface_scores, 0, fn {key, value}, acc ->
        weight = Map.get(policy.weights, key, 1)
        acc + value * weight
      end)

    tag_bonus =
      tags
      |> Enum.map(&Map.get(policy.tag_weights, &1, 0))
      |> Enum.sum()

    profile_bonus = profile_bonus(profile, policy.profiles, surface_scores)
    base + tag_bonus + profile_bonus
  end

  defp profile_bonus(profile, profiles, surface_scores) do
    profile_weights = Map.get(profiles, profile, %{})

    Enum.reduce(profile_weights, 0, fn {key, value}, acc ->
      acc + Map.get(surface_scores, key, 0) * value
    end)
  end

  defp state_vault_path("state_vault", state) do
    Map.get(state, "vault_path")
  end

  defp state_vault_path(_profile, _state), do: nil

  defp maybe_capture_state(decision, "state_vault", policy, input) do
    vault_path = state_vault_path("state_vault", policy.state)
    state_paths = StatePaths.resolve(input, policy.state)
    dry_run = Map.get(input, "dry_run", true)

    if vault_path && state_paths != [] do
      {:ok, vault_entry} =
        StateVault.capture(decision["operation_id"], decision["package_id"], state_paths,
          vault_path: vault_path,
          dry_run: dry_run
        )

      decision
      |> Map.put("state_vault_path", vault_path)
      |> Map.put("state_vault_entry", vault_entry.entry_dir)
      |> Map.put("state_vault_manifest", vault_entry.manifest)
    else
      Map.put(decision, "state_vault_path", vault_path)
    end
  end

  defp maybe_capture_state(decision, _profile, _policy, _input) do
    Map.put(decision, "state_vault_path", nil)
  end

  defp candidate_meta_with_images(candidates, candidate_meta) do
    Enum.reduce(candidates, candidate_meta, fn surface, acc ->
      if Map.has_key?(acc, surface) do
        acc
      else
        image = ContainerImagePicker.preferred_image(surface)

        if image do
          Map.put(acc, surface, %{"image" => image})
        else
          acc
        end
      end
    end)
  end

  defp default_operation_id do
    "op_" <> (DateTime.utc_now() |> DateTime.to_unix() |> Integer.to_string())
  end

  defp emit_telemetry(decision) do
    :telemetry.execute(
      [:dnfinition, :placement, :decision],
      %{count: 1},
      %{
        operation_id: decision["operation_id"],
        package_id: decision["package_id"],
        intent: decision["intent"],
        profile: decision["profile"],
        selected_surface: decision["selected_surface"],
        dry_run: decision["dry_run"],
        result: decision["result"]
      }
    )
  end
end
