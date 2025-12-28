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

  allowUnfree = true;

  permittedInsecurePackages = [
    "openssl-1.1.1w" # Temporary for AZ
    "python3.13-ecdsa-0.19.1"
  ];

  rocmSupport = true; # AMD Cuda suppor
}
