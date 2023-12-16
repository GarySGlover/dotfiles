{pkgs, ...}: {
  home.packages = with pkgs; [
    prusa-slicer
    unzip
    jdk21
    zip

    wlr-randr

    rofi

    age
    sops
    ssh-to-age
    git-crypt # Secrets management
  ];
}
