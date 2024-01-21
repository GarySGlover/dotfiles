{
  config,
  pkgs,
  lib,
  ...
}:
with lib; {
  config = mkIf config.wolf.languages.nim {
    home.packages = with pkgs; [
      # nim
      # nimble
      # pico-sdk
      # gnumake
      # clang
      # libclang
      # cmake
      # gcc-arm-embedded
    ];
  };
}
