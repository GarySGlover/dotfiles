{
  config,
  pkgs,
  lib,
  ...
}:
with lib; {
  config = mkIf config.wolf.languages.zig {
    home.packages = with pkgs; [
      zig
    ];
  };
}
