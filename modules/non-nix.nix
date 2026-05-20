# [[file:../modules.org::*Non-nix][Non-nix:1]]
{
  flake.aspects = {
    nonNix.homeManager =
      { pkgs, ... }:
      {
        nix = {
          package = pkgs.nix;
          extraOptions = ''
            experimental-features = nix-command flakes
          '';
        };
        home.packages = with pkgs; [
          # Might need to install nscnd with apt for some stuff to work
          nixgl.nixGLIntel
          niri
          physlock
        ];
      };
  };
}
# Non-nix:1 ends here
