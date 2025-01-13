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
      "steam-unwrapped"
      "terraform"
      "enhancer-for-youtube"
      "nvidia-x11"
      "nvidia-settings"
    ];

  permittedInsecurePackages = [
    "openssl-1.1.1w" # Temporary for AZ
  ];

  rocmSupport = true; # AMD Cuda suppor
}
