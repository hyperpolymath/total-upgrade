# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell <jonathan@hyperpolymath.io>

defmodule DNFinition.Placement.ContainerImagePicker do
  @moduledoc """
  Detect available container images and select a preferred base.
  """

  @runtimes %{
    "podman_oci" => {"podman", ["images", "--format", "json"]},
    "nerdctl" => {"nerdctl", ["images", "--format", "json"]}
  }

  def preferred_image(surface) do
    case Map.get(@runtimes, surface) do
      nil -> nil
      {cmd, args} -> pick_from_images(cmd, args)
    end
  end

  defp pick_from_images(cmd, args) do
    case System.cmd(cmd, args, stderr_to_stdout: true) do
      {output, 0} ->
        output
        |> decode_images()
        |> pick_preferred()

      _ ->
        nil
    end
  end

  defp decode_images(output) do
    case Jason.decode(output) do
      {:ok, images} when is_list(images) -> Enum.flat_map(images, &extract_names/1)
      _ -> []
    end
  end

  defp extract_names(%{"Names" => names}) when is_list(names), do: names

  defp extract_names(%{"Repository" => repo, "Tag" => tag})
       when is_binary(repo) and is_binary(tag) do
    ["#{repo}:#{tag}"]
  end

  defp extract_names(_), do: []

  defp pick_preferred(names) do
    Enum.find(names, &String.contains?(&1, "distroless")) ||
      Enum.find(names, &String.contains?(&1, "wolfi"))
  end
end
