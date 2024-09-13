{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkIf;
in
{
  config = mkIf config.wolf.roles.gaming {
    home.packages = with pkgs; [
      mangohud
    ];
  };
}
