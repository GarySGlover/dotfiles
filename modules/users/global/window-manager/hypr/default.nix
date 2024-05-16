{
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (lib) mkIf;
in {
  config = mkIf config.wolf.roles.desktop {
    wayland.windowManager.hyprland = {
      enable = true;
      extraConfig = builtins.readFile ./hyprland.conf;
    };

    home.file."${config.xdg.configHome}/hypr/imports" = {
      source = ./imports;
      recursive = true;
    };

    home.packages = with pkgs; [
      rofi-wayland # Application launcher
      swww # Animated wallpapers
      udiskie # Disk auto mount
      wlr-randr # Wlroots randr alternative
      brightnessctl
    ];
  };
}
