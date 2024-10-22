{
  config,
  pkgs,
  lib,
  ...
}:
let
  inherit (lib) mkIf;
in
{
  config = mkIf config.wolf.roles.desktop {
    home.file."${config.xdg.configHome}/kanshi/config" = {
      source = ./config;
    };

    home.packages = with pkgs; [
      kanshi
    ];
  };
}
