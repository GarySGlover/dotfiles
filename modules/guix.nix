# [[file:../modules.org::*Guix][Guix:1]]
{
  flake.aspects.guix = {
    homeManager =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [
          guix
        ];
      };
    nixos = {
      services.guix = {
        enable = true;
        gc = {
          enable = true;
          dates = "weekly";
          extraArgs = [
            "--delete-generations=1m"
            "--vacuum-database"
          ];
        };
      };
    };
  };
}
# Guix:1 ends here
