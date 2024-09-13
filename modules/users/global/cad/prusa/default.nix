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
  config = mkIf config.wolf.roles.cad {
    home.packages = with pkgs; [
      prusa-slicer
    ];
  };
}
