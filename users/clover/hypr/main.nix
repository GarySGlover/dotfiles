{
  config,
  pkgs,
  lib,
  ...
}: {
  config = {
    wayland.windowManager.hyprland = {
      enable = true;
      extraConfig = ''
        source = ~/.config/hypr/main.conf
        source = ~/.config/hypr/host.conf
      '';
    };

    home.file."${config.xdg.configHome}/hypr/main.conf".source = ./main.conf;
    home.file."${config.xdg.configHome}/hypr/host.conf".source = ../../../hosts + "/${config.wolf.hostname}/hypr.conf";
    home.packages = with pkgs; [
      swww # Animated wallpapers
    ];
  };
}
