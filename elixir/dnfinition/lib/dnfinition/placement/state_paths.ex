# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell <jonathan@hyperpolymath.io>

defmodule DNFinition.Placement.StatePaths do
  @moduledoc """
  Resolve state paths from input or policy defaults.
  """

  def resolve(input, policy_state) do
    case Map.get(input, "state_paths") do
      nil -> resolve_from_policy(input, policy_state)
      paths -> paths
    end
  end

  defp resolve_from_policy(input, policy_state) do
    intent = Map.get(input, "intent", "other")
    package_id = Map.get(input, "package_id", "unknown")

    paths_by_intent = Map.get(policy_state, "paths_by_intent", %{})
    paths_by_package = Map.get(policy_state, "paths_by_package", %{})

    case Map.get(paths_by_package, package_id) || Map.get(paths_by_intent, intent) do
      nil -> []
      paths -> Enum.map(paths, &expand_template(&1, package_id))
    end
  end

  defp expand_template(path, package_id) do
    String.replace(path, "{package_id}", package_id)
  end
end
