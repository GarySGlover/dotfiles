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
  config = {
    home.packages = with pkgs; [
      shellcheck
    ];
  };
}
