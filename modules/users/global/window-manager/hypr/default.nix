{
  config,
  pkgs,
  lib,
  ...
}:
with lib;
{
  config = mkIf config.wolf.roles.desktop {
    wayland.windowManager.hyprland = {
      enable = true;
      extraConfig = builtins.readFile ./hyprland.conf;
      plugins = [ pkgs.hyprlandPlugins.hy3 ];
    };

    home.file."${config.xdg.configHome}/hypr/imports" = {
      source = ./imports;
      recursive = true;
    };

    home.packages = with pkgs; [
      (writeShellScriptBin "wm-hyprland-test-config" (builtins.readFile ./wm-hyprland-test-config.sh))
      brightnessctl
      rofi-wayland # Application launcher
      swww # Animated wallpapers
      udiskie # Disk auto mount
      wlr-randr # Wlroots randr alternative
    ];
  };
}
