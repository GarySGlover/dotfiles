{
  pkgs,
  lib,
  ...
}:

with lib;

{
  config = {
    # allowUnfree = true;

    environment.systemPackages = with pkgs; [
      pciutils
      sops
      ags
    ];

    boot.initrd.availableKernelModules = [
      "nvme"
      "xhci_pci"
      "usbhid"
      "thunderbolt"
    ];

    boot.kernelParams = [
      # The GPD Pocket3 uses a tablet OLED display, that is mounted rotated 90° counter-clockwise
      "fbcon=rotate:1"
      "video=DSI-1:panel_orientation=right_side_up"
    ];

    services.xserver.videoDrivers = mkOverride 40 [
      "virtualbox"
      "vmware"
      "cirrus"
      "vesa"
      "modesetting"
      "nvidia"
    ];

    hardware.nvidia.open = false;
    hardware.graphics.extraPackages = with pkgs; [ intel-vaapi-driver ];

    nix.extraOptions = ''
      experimental-features = nix-command flakes
    '';

    # Set your time zone.
    time.timeZone = "Europe/London";

    # Select internationalisation properties.
    i18n.defaultLocale = "en_GB.UTF-8";
    console = {
      font = "Lat2-Terminus16";
      keyMap = "uk";
    };
  };
}
