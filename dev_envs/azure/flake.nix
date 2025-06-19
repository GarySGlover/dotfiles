{
  description = "Azure Shell Environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        devShells.default = pkgs.mkShell {
          name = "azure_devops";
          packages = with pkgs;
            [
              (azure-cli.withExtensions (with pkgs.azure-cli-extensions; [
                azure-devops
                aks-preview
                subscription
              ]))
            ];
        };
      });
}
