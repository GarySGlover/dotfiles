{
  pkgs,
  config,
  lib,
  ...
}: let
  inherit (lib) mkIf;
in {
  config = mkIf config.wolf.roles.desktop {
    home.packages = with pkgs; [
      waybar
    ];

    home.file."${config.xdg.configHome}/waybar" = {
      source = ./config;
      recursive = true;
    };
  };
}
