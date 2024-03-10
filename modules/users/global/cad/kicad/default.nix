{
  config,
  lib,
  pkgs,
  ...
}: let
  kicad = pkgs.kicad.override {
    addons = with pkgs.kicadAddons; [kikit kikit-library];
  };
in
  with lib; {
    # config = mkIf config.wolf.roles.cad {
    #   home.packages = [
    #     kicad
    #   ];
    # };
  }
