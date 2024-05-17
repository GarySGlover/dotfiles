{config, ...}: let
  homeDir = config.home.homeDirectory;
in {
  config = {
    programs.ssh = {
      enable = true;
      addKeysToAgent = "yes";
      forwardAgent = true;
      compression = true;
      matchBlocks = {
        "ssh.dev.azure.com" = {
          hostname = "ssh.dev.azure.com";
          identityFile = "${homeDir}/.ssh/id_ed25519_dev.azure.com";
        };
        "github.com" = {
          hostname = "github.com";
          identityFile = "${homeDir}/.ssh/id_ed25519_github.com";
        };
      };
    };

    services.ssh-agent.enable = true;
  };
}
