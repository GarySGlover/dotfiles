{ config, pkgs, ... }:
let
  homeDir = config.home.homeDirectory;
  sshKeygenScript = pkgs.writeShellScript "generate-ssh-keys" ''
    set -eu

    mkdir -p "${homeDir}/.ssh"
    chmod 700 "${homeDir}/.ssh"

    # Generate generic RSA key if missing
    if [ ! -f "${homeDir}/.ssh/id_rsa" ]; then
      ssh-keygen -t rsa -b 4096 -N "" -f "${homeDir}/.ssh/id_rsa"
    fi

    # Generate generic Ed25519 key if missing
    if [ ! -f "${homeDir}/.ssh/id_ed25519" ]; then
      ssh-keygen -t ed25519 -N "" -f "${homeDir}/.ssh/id_ed25519"
    fi
  '';
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
        "192.168.*.*".extraOptions = {
          StrictHostKeyChecking = "no";
          UserKnownHostsFile = "/dev/null";
        };
      };
    };

    services.ssh-agent.enable = true;

    systemd.user.services.generate-ssh-keys = {
      Unit = {
        Description = "Generate SSH keys if missing";
        After = [ "default.target" ];
      };
      Service = {
        Type = "oneshot";
        ExecStart = "${sshKeygenScript}";
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
    };
  };
}
