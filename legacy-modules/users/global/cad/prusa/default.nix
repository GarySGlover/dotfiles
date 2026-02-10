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
  config = {
    home.packages = with pkgs; [
      prusa-slicer
    ];
  };
}
