{
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (lib) mkIf;
in {
  config = mkIf config.wolf.languages.zig {
    home.packages = with pkgs; [
      zig
    ];
  };
}
