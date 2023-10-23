{lib, ...}: {
  # Enable Steam for gaming
  packageOverrides = pkgs: {
    steam = pkgs.steam.override {
      extraPkgs = pkgs:
        with pkgs; [
          libgdiplus
          keyutils
          libkrb5
          libpng
          libpulseaudio
          libvorbis
          stdenv.cc.cc.lib
          xorg.libXcursor
          xorg.libXi
          xorg.libXinerama
          xorg.libXScrnSaver
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
      ];
}
