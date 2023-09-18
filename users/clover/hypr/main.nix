{
  config,
  pkgs,
  lib,
  ...
}: {
  wayland.windowManager.hyprland = {
    enable = true;
    extraConfig = ''
      source = ~/.config/hypr/main.conf
      source = ~/.config/hypr/host.conf
    '';
  };

  home.file."${config.xdg.configHome}/hypr/main.conf".source = ../hypr/main.conf;

  home.packages = with pkgs; [
    swww # Animated wallpapers
  ];
}
