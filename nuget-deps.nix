# NuGet package dependencies for Nix builds.
# This file is consumed by buildDotnetModule to fetch packages in the Nix sandbox.
#
# Regenerate after adding/removing packages:
#   nix build .#roblang.fetch-deps
#   ./result nuget-deps.nix
{ fetchNuGet }: [
  # No external NuGet packages — FSharp.Core is bundled with the SDK.
]
