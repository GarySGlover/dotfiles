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
  config = mkIf (config.wolf.roles.internet && config.wolf.roles.work) {
    home.packages = with pkgs; [
      chromium
    ];
  };
}
