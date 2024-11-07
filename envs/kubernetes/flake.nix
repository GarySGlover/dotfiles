{
  description = "Flake for direnv";
  inputs.nixpkgs.url = "nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.permittedInsecurePackages = [
            "openssl-1.1.1w" # Used by one of the Azure Cli packages
          ];
        };
        local-pkgs = import ../../packages/packages.nix { inherit pkgs; };

        lib = pkgs.lib;
        excludedExtensions =
          [ "connection-monitor-preview" "blockchain" "aks-preview" ];

        isDesiredExtension = item:
          let
            name = item.pname or "";
            result = builtins.tryEval item;
          in result.success && !(builtins.elem name excludedExtensions)
          && lib.isDerivation item;

        azure-cli = pkgs.azure-cli.withExtensions
          (builtins.filter isDesiredExtension
            (lib.attrValues pkgs.azure-cli-extensions));

        packages = with pkgs; [
          pre-commit
          # Nix packages
          nixfmt
          nixd
          # Kubernetes
          argo-rollouts
          helm-dashboard
          k9s
          kubectl
          kubernetes-helm
          # Packages for azure packages
          kubelogin

          local-pkgs.ak9s
        ];
      in {
        devShells.default =
          pkgs.mkShell { packages = packages ++ [ azure-cli ]; };
      });
}
