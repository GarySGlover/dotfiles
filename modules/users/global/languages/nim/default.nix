{
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (lib) mkIf;
in {
  config = mkIf config.wolf.languages.nim {
    home.packages = with pkgs; [
      nim
      nimble
    ];
  };
}
