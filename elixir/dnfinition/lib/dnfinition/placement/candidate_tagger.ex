# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell <jonathan@hyperpolymath.io>

defmodule DNFinition.Placement.CandidateTagger do
  @moduledoc """
  Derive candidate tags from surface names and metadata.
  """

  def tag_candidates(candidates, candidate_meta \\ %{}) do
    Enum.reduce(candidates, %{}, fn surface, acc ->
      meta = Map.get(candidate_meta, surface, %{})
      tags = Enum.uniq(surface_tags(surface) ++ meta_tags(meta))
      Map.put(acc, surface, tags)
    end)
  end

  defp surface_tags(surface) do
    tags = []
    tags = add_if(tags, surface, "toolbox", "toolbox")
    tags = add_if(tags, surface, "distrobox", "distrobox")
    tags = add_if(tags, surface, "podman", "container")
    tags = add_if(tags, surface, "nerdctl", "container")
    tags = add_if(tags, surface, "vm", "vm")
    tags = add_if(tags, surface, "user", "user_scope")
    tags = add_if(tags, surface, "system", "system_scope")
    tags
  end

  defp meta_tags(%{"image" => image}) when is_binary(image) do
    tags = []
    tags = add_if(tags, image, "distroless", "distroless")
    tags = add_if(tags, image, "wolfi", "wolfi")
    tags
  end

  defp meta_tags(_), do: []

  defp add_if(tags, haystack, needle, tag) do
    if String.contains?(haystack, needle) do
      [tag | tags]
    else
      tags
    end
  end
end
