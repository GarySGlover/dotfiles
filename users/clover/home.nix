{config, pkgs, ...}:

{
  # Home manager instal and configure self
  programs.home-manager.enable = true;

  home.username = "clover";
  home.homeDirectory = "/home/clover";
  home.stateVersion = "23.05";

  imports = [
    ./emacs.nix
  ];

  home.packages = with pkgs; [
    firefox
    git pre-commit
    go python311
    wlr-randr
    alacritty
    age sops ssh-to-age # Nix-sops secrets management
  ];

  fonts.fontconfig.enable = true;
}
