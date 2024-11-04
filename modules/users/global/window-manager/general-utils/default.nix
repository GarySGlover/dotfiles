{
  config,
  pkgs,
  lib,
  ...
}:
with lib;
{
  config = mkIf config.wolf.roles.desktop {
    home.packages = with pkgs; [
      brightnessctl
      flameshot
      hyprshot
      nwg-displays
      rofi # Application launcher
      swww # Animated wallpapers
      udiskie # Disk auto mount
      wlr-randr # Wlroots randr alternative
    ];
  };
}
