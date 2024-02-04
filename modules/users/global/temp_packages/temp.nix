{pkgs, ...}: {
  home.packages = with pkgs; [
    prusa-slicer
    unzip
    jdk21
    zip
    glxinfo
    wlr-randr
    nwg-displays
    pandoc
  ];
}
