{ lib, ... }:
let
  inherit (lib) elem getName;
in
{
  # Enable Steam for gaming
  packageOverrides = pkgs: {
    steam = pkgs.steam.override {
      extraPkgs =
        pkgs: with pkgs; [
          keyutils
          libgdiplus
          libkrb5
          libpng
          libpulseaudio
          libvorbis
          stdenv.cc.cc.lib
          xorg.libXScrnSaver
          xorg.libXcursor
          xorg.libXi
          xorg.libXinerama
        ];
    };
  };

  allowUnfreePredicate =
    pkg:
    elem (getName pkg) [
      "aspell-dict-en-science"
      "codeium"
      "steam"
      "steam-original"
      "steam-run"
      "terraform"
    ];

  rocmSupport = true; # AMD Cuda support
}
