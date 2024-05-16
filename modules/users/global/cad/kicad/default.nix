{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkIf;
  kicad = pkgs.kicad.override {
    addons = with pkgs.kicadAddons; [kikit kikit-library];
  };
in {
  config = mkIf config.wolf.roles.cad {
    home.packages = [
      kicad
    ];
  };
}
