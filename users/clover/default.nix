{
  config,
  pkgs,
  ...
}: let
  homeDir = config.home.homeDirectory;
in {
  # Home manager install and configure self
  programs.home-manager.enable = true;

  home.stateVersion = "23.05";

  imports = [
    ./hypr/main.nix
    ./terminal.nix
  ];

  home.packages = with pkgs; [
    firefox
    wlr-randr

    brave

    rofi

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
