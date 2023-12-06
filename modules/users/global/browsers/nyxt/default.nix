{
  config,
  lib,
  pkgs,
  ...
}:
with lib; {
  config = mkIf config.wolf.roles.internet {
    home.packages = with pkgs; [
      nyxt
    ];
  };
}
