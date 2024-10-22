{
  config,
  pkgs,
  lib,
  ...
}:
with lib;
{
  config = mkIf config.wolf.roles.desktop {
    home.file."${config.xdg.configHome}/sway/config" = {
      source = ./config;
    };

    home.packages = with pkgs; [
      (writeShellScriptBin "wm-bar" (builtins.readFile ./wm-bar.sh))
      (writeShellScriptBin "wm-workspace-switch" (builtins.readFile ./wm-workspace-switch.sh))
      brightnessctl
      nwg-displays
      udiskie # Disk auto mount
    ];
  };
}
