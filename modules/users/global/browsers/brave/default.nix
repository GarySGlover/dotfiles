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
  config = mkIf config.wolf.roles.internet {
    home.packages = with pkgs; [
      brave
    ];
  };
}
