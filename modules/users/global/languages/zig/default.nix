{
  config,
  pkgs,
  lib,
  ...
}:
with lib; {
  config = mkIf config.wolf.languages.go {
    home.packages = with pkgs; [
      zig
    ];
  };
}
