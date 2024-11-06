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
      hyprshot
      udiskie # Disk auto mount
    ];
  };
}
