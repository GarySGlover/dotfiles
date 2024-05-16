{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkIf;
in {
  config = mkIf config.wolf.roles.programming {
    home.packages = with pkgs; [
      qmk
    ];
  };
}
