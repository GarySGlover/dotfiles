{config, pkgs, pkgs-unstable, ...}:

let
  homeDir = "/home/clover";
in {
  # Home manager install and configure self
  programs.home-manager.enable = true;

  home.username = "clover";
  home.homeDirectory = homeDir;
  home.stateVersion = "23.05";

  imports = [
    ./fonts.nix
    ./emacs.nix
    ./hyprland.nix
    ./terminal.nix
  ];

  home.packages = with pkgs; [
    firefox
    git pre-commit
    go python311
    wlr-randr

    age sops ssh-to-age # Nix-sops secrets management
  ];

  programs.ssh.enable = true;
  programs.ssh.matchBlocks = {
    "dev.azure.com" = {
      hostname = "dev.azure.com";
      identityFile = "${homeDir}/.ssh/id_ed25519_dev.azure.com";
    };
    "github.com" = {
      hostname = "github.com";
      identityFile = "${homeDir}/.ssh/id_ed25519_github.com";
    };
  };
}
