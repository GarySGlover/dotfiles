{ pkgs, ... }:

{
  # Default sops config
  sops.age.sshKeyPaths = [ "/etc/ssh/ssh_auberon_ed25519" ];

  imports =
  [
    ./hardware-configuration.nix
    ../global
    ../global/wifi.nix
    ../global/efi-bootloader.nix
    ../global/desktop.nix
    ./users.nix
  ];

  # Hostname
  networking.hostName = "auberon";

  # Bluetooth enable
  hardware.bluetooth.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  services.xserver.libinput.enable = true;
}
