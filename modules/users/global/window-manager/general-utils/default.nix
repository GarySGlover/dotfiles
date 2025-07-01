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
      udiskie # Disk auto mount
    ];
  };
}
