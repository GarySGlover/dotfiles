{
  config,
  pkgs,
  lib,
  hostname,
  ...
}: {
  wayland.windowManager.hyprland = {
    enable = true;
    extraConfig = ''
      source = ~/.config/hypr/main.conf
      source = ~/.config/hypr/host.conf
    '';
  };

  home.file."${config.xdg.configHome}/hypr/main.conf".source = ./main.conf;
  home.file."${config.xdg.configHome}/hypr/host.conf".source = ../../../hosts + "/${hostname}/hypr.conf";
  home.packages = with pkgs; [
    swww # Animated wallpapers
  ];
}
