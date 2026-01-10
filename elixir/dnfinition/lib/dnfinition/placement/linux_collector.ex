# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell <jonathan@hyperpolymath.io>

defmodule DNFinition.Placement.LinuxCollector do
  @moduledoc """
  Linux-wide candidate and baseline score collection, including WSL detection.
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
      |> maybe_add("rpm_ostree_host", command?("rpm-ostree"))
      |> maybe_add("dnf", command?("dnf"))
      |> maybe_add("apt", command?("apt"))
      |> maybe_add("zypper", command?("zypper"))
      |> maybe_add("pacman", command?("pacman"))
      |> maybe_add("xbps", command?("xbps-install"))
      |> maybe_add("apk", command?("apk"))
      |> maybe_add("flatpak_user", command?("flatpak"))
      |> maybe_add("snap", command?("snap"))
      |> maybe_add("nix", command?("nix"))
      |> maybe_add("guix", command?("guix"))
      |> maybe_add("homebrew", command?("brew"))
      |> maybe_add("toolbox", command?("toolbox"))
      |> maybe_add("distrobox", command?("distrobox"))
      |> maybe_add("podman_oci", command?("podman"))
      |> maybe_add("nerdctl", command?("nerdctl"))
      |> maybe_add("asdf", command?("asdf"))

    case intent do
      "gui_app" -> candidates ++ ["appimage"]
      "toolchain" -> candidates
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
      "snap" -> bump(scores, %{"functional_fit" => 1})
      "appimage" -> bump(scores, %{"functional_fit" => 1, "clean_removal" => -1})
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

  defp surface_bias(scores, "rpm_ostree_host"), do: bump(scores, %{"reversibility" => 1})
  defp surface_bias(scores, "nix"), do: bump(scores, %{"reversibility" => 2})
  defp surface_bias(scores, "guix"), do: bump(scores, %{"reversibility" => 2})
  defp surface_bias(scores, "podman_oci"), do: bump(scores, %{"containability" => 1})
  defp surface_bias(scores, "nerdctl"), do: bump(scores, %{"containability" => 1})
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

  defp environment do
    %{
      "os_family" => os_family(),
      "os_name" => os_name(),
      "os_like" => os_like(),
      "wsl" => wsl?(),
      "container" => container?(),
      "virtual_machine" => vm?(),
      "minix" => minix?()
    }
  end

  defp wsl? do
    content =
      ["/proc/sys/kernel/osrelease", "/proc/version"]
      |> Enum.find_value("", fn path ->
        case File.read(path) do
          {:ok, data} -> data
          _ -> nil
        end
      end)

    String.contains?(String.downcase(content), "microsoft")
  end

  defp container? do
    cgroup =
      case File.read("/proc/1/cgroup") do
        {:ok, data} -> data
        _ -> ""
      end

    markers = ["docker", "kubepods", "containerd", "podman", "lxc", "libpod"]

    markers
    |> Enum.any?(fn marker -> String.contains?(cgroup, marker) end)
  end

  defp vm? do
    case System.cmd("systemd-detect-virt", ["--vm"]) do
      {_, 0} -> true
      _ -> false
    end
  end

  defp minix? do
    String.match?(String.upcase(os_family()), ~r/MINIX/)
  end

  defp os_family do
    case System.cmd("uname", ["-s"]) do
      {name, 0} -> String.trim(name)
      _ -> "unknown"
    end
  end

  defp os_name do
    os_release()
    |> Map.get("ID")
    |> case do
      nil -> os_family()
      value -> value
    end
  end

  defp os_like do
    os_release()
    |> Map.get("ID_LIKE")
    |> case do
      nil -> ""
      value -> value
    end
  end

  defp os_release do
    case File.read("/etc/os-release") do
      {:ok, content} -> parse_os_release(content)
      _ -> %{}
    end
  end

  defp parse_os_release(content) do
    content
    |> String.split("\n", trim: true)
    |> Enum.reduce(%{}, fn line, acc ->
      case String.split(line, "=", parts: 2) do
        [key, value] ->
          cleaned = value |> String.trim() |> String.trim("\"")
          Map.put(acc, key, cleaned)

        _ ->
          acc
      end
    end)
  end
end
