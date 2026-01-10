# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell <jonathan@hyperpolymath.io>

defmodule DNFinition.Placement.KinoiteCollector do
  @moduledoc """
  Kinoite-focused candidate and baseline score collection.
  """

  def collect(intent) do
    candidates = base_candidates(intent)
    scores = Enum.into(candidates, %{}, &{&1, baseline_scores(intent, &1)})

    %{
      "candidates" => candidates,
      "scores" => scores
    }
  end

  defp base_candidates(intent) do
    candidates = ["rpm_ostree_host"]
    candidates = maybe_add(candidates, "flatpak_user", command?("flatpak"))
    candidates = maybe_add(candidates, "toolbox", command?("toolbox"))
    candidates = maybe_add(candidates, "distrobox", command?("distrobox"))
    candidates = maybe_add(candidates, "podman_oci", command?("podman"))
    candidates = maybe_add(candidates, "nerdctl", command?("nerdctl"))

    case intent do
      "gui_app" -> candidates ++ ["appimage"]
      "service" -> candidates
      "toolchain" -> candidates ++ ["asdf"]
      _ -> candidates
    end
    |> Enum.uniq()
  end

  defp baseline_scores(intent, surface) do
    base = %{
      "dependability" => 3,
      "security" => 3,
      "interoperability" => 3,
      "functional_fit" => 3,
      "containability" => 3,
      "performance" => 3,
      "efficiency" => 3,
      "reversibility" => 3,
      "clean_removal" => 3,
      "state_retention" => 2
    }

    base
    |> intent_bias(intent, surface)
    |> surface_bias(surface)
  end

  defp intent_bias(scores, "gui_app", surface) do
    case surface do
      "flatpak_user" -> bump(scores, %{"functional_fit" => 2, "containability" => 1})
      "appimage" -> bump(scores, %{"functional_fit" => 1, "clean_removal" => -1})
      _ -> scores
    end
  end

  defp intent_bias(scores, "service", surface) do
    case surface do
      "nerdctl" -> bump(scores, %{"containability" => 2})
      "podman_oci" -> bump(scores, %{"containability" => 2})
      _ -> scores
    end
  end

  defp intent_bias(scores, "toolchain", surface) do
    case surface do
      "asdf" -> bump(scores, %{"functional_fit" => 2, "state_retention" => 1})
      "toolbox" -> bump(scores, %{"containability" => 1})
      _ -> scores
    end
  end

  defp intent_bias(scores, _intent, _surface), do: scores

  defp surface_bias(scores, "rpm_ostree_host") do
    bump(scores, %{"reversibility" => 1, "containability" => -1})
  end

  defp surface_bias(scores, "nerdctl"), do: bump(scores, %{"containability" => 1})
  defp surface_bias(scores, "podman_oci"), do: bump(scores, %{"containability" => 1})
  defp surface_bias(scores, "toolbox"), do: bump(scores, %{"containability" => 1})
  defp surface_bias(scores, _), do: scores

  defp bump(scores, deltas) do
    Enum.reduce(deltas, scores, fn {key, delta}, acc ->
      Map.update(acc, key, delta, &(&1 + delta))
    end)
  end

  defp command?(name) do
    System.find_executable(name) != nil
  end

  defp maybe_add(list, item, true), do: [item | list]
  defp maybe_add(list, _item, false), do: list
end
