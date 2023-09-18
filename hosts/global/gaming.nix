{pkgs, ...}: {
  # Enable Steam for gaming
  nixpkgs.config.packageOverrides = pkgs: {
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
  environment.systemPackages = with pkgs; [
    steam
    seatd
    mangohud
  ];
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
  };
  programs.gamescope.enable = true;
}
