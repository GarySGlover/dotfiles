{ pkgs, config, lib, ... }:

with lib;

{
  environment.systemPackages = with pkgs; [
    pciutils
    sops
    ags
  ];

  boot.kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;

  services.xserver.videoDrivers = mkOverride 40 [ "virtualbox" "vmware" "cirrus" "vesa" "modesetting" ];

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
}
