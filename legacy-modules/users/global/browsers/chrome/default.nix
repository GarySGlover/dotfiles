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
      chromium
      google-chrome
    ];
  };
}
