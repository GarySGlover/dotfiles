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
      '';
    };

    home.file."${config.xdg.configHome}/hypr/main.conf".source = ./main.conf;
    home.packages = with pkgs; [
      swww # Animated wallpapers
    ];
  };
}
