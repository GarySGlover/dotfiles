# [[file:../../modules.org::*Nix][Nix:1]]
{
  flake.aspects.core = {
    homeManager = { pkgs, ... }:
    {
      home.packages = with pkgs; [
        nix
        nh
        nix-index
      ];
    };
    nixos = {};
  };
}
# Nix:1 ends here
