{ config, ... }:
let
  homeDir = config.home.homeDirectory;
in
{
  config = {
    programs.ssh = {
      enableDefaultConfig = false;
      enable = true;
      matchBlocks = {
        "*" = {
          addKeysToAgent = "yes";
          compression = true;
          forwardAgent = true;
        };
        "ssh.dev.azure.com" = {
          hostname = "ssh.dev.azure.com";
          identityFile = "${homeDir}/.ssh/id_rsa_dev.azure.com";
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
