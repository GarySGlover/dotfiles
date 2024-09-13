{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkIf;
  # kicad = pkgs.kicad.override {
  #   addons = with pkgs.kicadAddons; [kikit kikit-library];
  # };
  kicad = pkgs.kicad;
in
{
  config = mkIf config.wolf.roles.cad {
    home.packages = [
      kicad
    ];
  };
}
