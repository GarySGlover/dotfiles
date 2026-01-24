{ pkgs, ... }:
[
  (pkgs.emacsPackages.trivialBuild rec {
    pname = "azure-devops";
    version = "1.0";
    src = ./azure-devops.el;
    propagatedUserEnvPkgs = [ ];
    buildInputs = propagatedUserEnvPkgs;
    nativeBuildInputs = with pkgs; [ git ];
  })
]
