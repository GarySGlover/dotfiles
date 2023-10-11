{pkgs, ...}: {
  imports = [
    ./hardware-configuration.nix
    ../global
    ../global/wifi.nix
    ../global/efi-bootloader.nix
    ../global/desktop.nix
    ../global/gaming.nix
  ];

  # Hostname
  networking.hostName = "belisarius";

  # Bluetooth enable
  hardware.bluetooth.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  services.xserver.libinput.enable = true;

  # ASUS Services
  services.asusd.enable = true;
}
