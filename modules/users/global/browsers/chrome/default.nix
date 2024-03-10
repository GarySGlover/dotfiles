{
  config,
  lib,
  pkgs,
  ...
}:
with lib; {
  config = mkIf (config.wolf.roles.internet
    && config.wolf.roles.work) {
    home.packages = with pkgs; [
      chromium
    ];
  };
}
