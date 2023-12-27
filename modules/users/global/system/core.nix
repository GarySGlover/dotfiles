{pkgs, ...}: {
  home.packages = with pkgs; [
    age
    git-crypt # Secrets management
    sops
    ssh-to-age
  ];
}
