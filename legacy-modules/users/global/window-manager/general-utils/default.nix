{
  config,
  pkgs,
  lib,
  ...
}:
with lib;
{
  config = {
    home.packages = with pkgs; [
      brightnessctl
      udiskie # Disk auto mount
    ];

    services.udiskie = {
      enable = true;
      automount = true;
    };
  };
}
