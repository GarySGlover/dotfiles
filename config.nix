{lib, ...}: {
  # Enable Steam for gaming
  packageOverrides = pkgs: {
    steam = pkgs.steam.override {
      extraPkgs = pkgs:
        with pkgs; [
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

  allowUnfreePredicate = with builtins;
  with lib;
    pkg:
      elem (getName pkg) [
        "steam"
        "steam-original"
        "steam-run"
        "terraform"
      ];

  rocmSupport = true; # AMD Cuda support
}
