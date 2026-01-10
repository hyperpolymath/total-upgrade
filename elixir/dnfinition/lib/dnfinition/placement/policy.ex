# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell <jonathan@hyperpolymath.io>

defmodule DNFinition.Placement.Policy do
  @moduledoc """
  Loads and normalizes placement policy configuration.
  """

  @default_path "/etc/dnfinition/placement_policy.toml"

  defstruct weights: %{},
            constraints: %{},
            overrides: %{},
            tag_weights: %{},
            profiles: %{},
            state: %{}

  def load(path \\ @default_path) do
    case Toml.decode_file(path) do
      {:ok, data} -> normalize(data)
      {:error, _} -> default()
    end
  end

  def default do
    %__MODULE__{
      weights: %{
        "dependability" => 3,
        "security" => 4,
        "interoperability" => 3,
        "functional_fit" => 4,
        "containability" => 4,
        "performance" => 2,
        "efficiency" => 2,
        "reversibility" => 3,
        "clean_removal" => 3,
        "state_retention" => 2,
        "auditability" => 3,
        "cost_risk" => 2
      },
      constraints: %{
        "kernel_module" => ["rpm_ostree_host"],
        "driver" => ["rpm_ostree_host"],
        "system_service" => ["rpm_ostree_host"],
        "gui_app" => ["flatpak_user", "flatpak_system"],
        "toolchain" => ["toolbox", "distrobox", "asdf"],
        "service" => ["nerdctl", "podman_oci"],
        "untrusted_app" => ["nerdctl", "podman_oci", "vm"],
        "containment_preferred" => ["nerdctl", "podman_oci", "toolbox", "distrobox", "vm"],
        "distroless_preferred" => ["distroless", "wolfi"]
      },
      overrides: %{},
      tag_weights: %{
        "container" => 3,
        "vm" => 2,
        "distroless" => 3,
        "wolfi" => 2
      },
      profiles: %{
        "zero_trace" => %{"containability" => 3, "clean_removal" => 3},
        "state_vault" => %{"state_retention" => 3},
        "multi_version" => %{"reversibility" => 2, "containability" => 2}
      },
      state: %{"vault_path" => "/var/lib/dnfinition/state-vault"}
    }
  end

  defp normalize(data) do
    %__MODULE__{
      weights: Map.get(data, "weights", %{}),
      constraints: Map.get(data, "constraints", %{}),
      overrides: Map.get(data, "overrides", %{}),
      tag_weights: Map.get(data, "tag_weights", %{}),
      profiles: Map.get(data, "profiles", %{}),
      state: Map.get(data, "state", %{})
    }
  end
end
