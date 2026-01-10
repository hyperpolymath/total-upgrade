# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell <jonathan@hyperpolymath.io>

defmodule DNFinition.Placement.BsdCollector do
  @moduledoc """
  BSD/Unix auto-collector stub (FreeBSD/OpenBSD/NetBSD).
  """

  def collect(intent) do
    candidates = base_candidates(intent)
    scores = Enum.into(candidates, %{}, &{&1, baseline_scores(intent, &1)})

    %{
      "candidates" => candidates,
      "scores" => scores,
      "environment" => environment()
    }
  end

  defp base_candidates(intent) do
    candidates =
      []
      |> maybe_add("pkg", command?("pkg"))
      |> maybe_add("pkg_add", command?("pkg_add"))
      |> maybe_add("pkgin", command?("pkgin"))
      |> maybe_add("ports", command?("portmaster"))
      |> maybe_add("flatpak_user", command?("flatpak"))
      |> maybe_add("nix", command?("nix"))
      |> maybe_add("guix", command?("guix"))

    case intent do
      "gui_app" -> candidates ++ ["appimage"]
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
  end

  defp intent_bias(scores, "gui_app", surface) do
    case surface do
      "flatpak_user" -> bump(scores, %{"functional_fit" => 2, "containability" => 1})
      "appimage" -> bump(scores, %{"functional_fit" => 1, "clean_removal" => -1})
      _ -> scores
    end
  end

  defp intent_bias(scores, _intent, _surface), do: scores

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

  defp environment do
    %{
      "os_family" => os_family()
    }
  end

  defp os_family do
    case System.cmd("uname", ["-s"]) do
      {name, 0} -> String.trim(name)
      _ -> "unknown"
    end
  end
end
