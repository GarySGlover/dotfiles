{
  config,
  pkgs,
  lib,
  ...
}:
with lib; {
  # https://nixos.wiki/wiki/NixOS_modules
  options = {
    clover = {
      hostname = mkOption {
        type = types.str;
      };
    };
  };

  config = {
    wayland.windowManager.hyprland = {
      enable = true;
      extraConfig = ''
        source = ~/.config/hypr/main.conf
        source = ~/.config/hypr/host.conf
      '';
    };

    home.file."${config.xdg.configHome}/hypr/main.conf".source = ./main.conf;
    home.file."${config.xdg.configHome}/hypr/host.conf".source = ../../../hosts + "/${config.clover.hostname}/hypr.conf";
    home.packages = with pkgs; [
      swww # Animated wallpapers
    ];
  };
}
