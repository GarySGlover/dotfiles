{config, ...}: {
  sops.age.sshKeyPaths = ["/etc/ssh/ssh_${config.networking.hostName}_ed25519"];
}
