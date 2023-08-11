{config, pkgs, pkgs-unstable, ...}:

{
  # Home manager install and configure self
  programs.home-manager.enable = true;

  home.username = "clover";
  home.homeDirectory = "/home/clover";
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
}
