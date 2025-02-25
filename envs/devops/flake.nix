{
  description = "Flake for direnv";
  inputs.nixpkgs.url = "nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        local-pkgs = import ../../packages/packages.nix { inherit pkgs; };

        azure-cli = pkgs.azure-cli.withExtensions
          (with pkgs.azure-cli-extensions; [
            azure-devops
            aks-preview
            subscription
          ]);

        packages = with pkgs; [
          pre-commit
          # Nix packages
          nixfmt
          nixd

          # Packages for Azure DevOps
          curl
          kubelogin
          local-pkgs.devops-clone
          local-pkgs.azure-tools
        ];
      in {
        devShells.default =
          pkgs.mkShell { packages = packages ++ [ azure-cli ]; };
      });
}
