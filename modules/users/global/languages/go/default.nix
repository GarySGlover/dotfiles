{
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (lib) mkIf;
in {
  config = mkIf config.wolf.languages.go {
    home.packages = with pkgs; [
      go
      gopls
      gotools
    ];
  };
}
