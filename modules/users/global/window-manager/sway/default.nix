{
  config,
  pkgs,
  lib,
  ...
}:
let
  inherit (lib) mkIf;
in
{
  config = mkIf config.wolf.roles.desktop {
    home.file."${config.xdg.configHome}/sway/config" = {
      source = ./config;
    };

    home.file."${config.xdg.configHome}/sway/bar.sh" = {
      source = ./bar.sh;
      executable = true;
    };

    home.packages = with pkgs; [
      brightnessctl
      nwg-displays
      udiskie # Disk auto mount
    ];
  };
}
