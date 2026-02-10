{ pkgs, ... }:
{
  home.packages = with pkgs; [
    age
    git-crypt # Secrets management
    nix-search-cli
    sops
    ssh-to-age
  ];
}
