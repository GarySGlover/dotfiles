# [[file:../../modules.org::*Nix][Nix:1]]
{
  flake.aspects.core = {
    homeManager =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [
          deadnix
          manix
          nh
          nix-index
          nix-tree
          nixd
          nixfmt
        ];
      };
    nixos = { };
  };
}
# Nix:1 ends here
