{config, pkgs, ...}:

{
  # Home manager instal and configure self
  programs.home-manager.enable = true;

  home.username = "clover";
  home.homeDirectory = "/home/clover";
  home.stateVersion = "23.05";


  home.packages = with pkgs; [
    firefox
    emacs
    emacsPackages.nix-mode
    git pre-commit
    go python311
    wlr-randr
    alacritty
    age sops ssh-to-age # Nix-sops secrets management
  ];
}
