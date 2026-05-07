{
  description = "RobLang programming language";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        dotnet-sdk = pkgs.dotnet-sdk_10;
        dotnet-runtime = pkgs.dotnet-runtime_10;

        commonDotnetArgs = {
          src = ./.;
          # Regenerate after adding/removing NuGet packages:
          #   nix build .#roblang-cli.fetch-deps && ./result nuget-deps.json
          nugetDeps = ./nuget-deps.nix;
          inherit dotnet-sdk dotnet-runtime;
        };

        roblang = pkgs.buildDotnetModule (commonDotnetArgs // {
          pname = "roblang";
          version = "0.1.0";
          projectFile = "src/RobLang/RobLang.fsproj";
          meta = with pkgs.lib; {
            description = "RobLang compiler library";
            license = licenses.mit;
          };
        });

        roblang-cli = pkgs.buildDotnetModule (commonDotnetArgs // {
          pname = "roblang-cli";
          version = "0.1.0";
          projectFile = "src/RobLang.Cli/RobLang.Cli.fsproj";
          executables = [ "RobLang.Cli" ];
          meta = with pkgs.lib; {
            description = "RobLang compiler CLI";
            license = licenses.mit;
            mainProgram = "RobLang.Cli";
          };
        });
      in
      {
        packages = {
          default = roblang-cli;
          inherit roblang roblang-cli;
        };

        apps.default = {
          type = "app";
          program = "${roblang-cli}/bin/RobLang.Cli";
        };

        checks = {
          build-lib = roblang;
          build-cli = roblang-cli;

          formatting = pkgs.runCommand "check-fmt" {
            nativeBuildInputs = [ pkgs.fantomas ];
          } ''
            fantomas --check ${./src}
            touch $out
          '';
        };

        devShells.default = pkgs.mkShell {
          packages = [
            dotnet-sdk
            pkgs.fsautocomplete   # F# language server (LSP)
            pkgs.fantomas         # F# formatter
            pkgs.nuget-to-json     # regenerate nuget-deps.nix
          ];

          DOTNET_CLI_TELEMETRY_OPTOUT = "1";
          DOTNET_NOLOGO = "1";
          DOTNET_ROOT = dotnet-sdk;

          shellHook = ''
            export NUGET_PACKAGES="$PWD/.nuget/packages"
          '';
        };
      }
    );
}
