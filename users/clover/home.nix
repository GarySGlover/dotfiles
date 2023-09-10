{
  config,
  pkgs,
  ...
}: let
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
    ./git.nix
  ];

  home.packages = with pkgs; [
    firefox
    pre-commit
    go
    python311
    wlr-randr

    age
    sops
    ssh-to-age
    git-crypt # Secrets management
  ];

  programs.ssh = {
    enable = true;
    extraConfig = "AddKeysToAgent yes";
    forwardAgent = true;
    compression = true;
    matchBlocks = {
      "dev.azure.com" = {
        hostname = "dev.azure.com";
        identityFile = "${homeDir}/.ssh/id_ed25519_dev.azure.com";
      };
      "github.com" = {
        hostname = "github.com";
        identityFile = "${homeDir}/.ssh/id_ed25519_github.com";
      };
    };
  };

  services.ssh-agent.enable = true;
}
