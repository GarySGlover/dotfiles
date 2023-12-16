{
  config,
  pkgs,
  lib,
  ...
}:
with lib; {
  config = mkIf config.wolf.roles.desktop {
    wayland.windowManager.hyprland = {
      enable = true;
      extraConfig = builtins.readFile ./hyprland.conf;
    };

    home.packages = with pkgs; [
      swww # Animated wallpapers
      udiskie # Disk auto mount
    ];
  };
}
