{
  description = "Flake for direnv";
  inputs.nixpkgs.url = "nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in {
        devShells.default = pkgs.mkShell {
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
            azure-cli
            kubelogin
          ];
        };
      });
}
