{pkgs, ...}: {
  home.packages = with pkgs; [
    unzip
    jdk21
    zip
    glxinfo
    wlr-randr
    nwg-displays
    pandoc
  ];
}
