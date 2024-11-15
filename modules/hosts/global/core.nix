{
  pkgs,
  host,
  self,
  ...
}:
{
  # Hostname
  networking.hostName = "${host}";

  # Enable touchpad support (enabled default in most desktopManager).
  services.libinput.enable = true;

  # Udisk2 Disk Auto Mounting
  services.udisks2.enable = true;

  # SSD Trimming
  services.fstrim.enable = true;

  # Default sops config
  sops.defaultSopsFile = "${self.outPath}/secrets/global.yaml";

  # Nix flakes
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  # Nix Garbage Collection
  nix.gc = {
    automatic = true;
    dates = "daily";
    options = "--delete-older-than 7d";
  };

  # Set your time zone.
  time.timeZone = "Europe/London";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_GB.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "uk";
  };

  # Force users to be configured by nix
  users.mutableUsers = false;

  # Kernel
  boot.kernelPackages = pkgs.linuxPackages_testing;

  # Clean tmp directory
  boot.tmp.cleanOnBoot = true;

  # Extra system packages
  environment.systemPackages = with pkgs; [
    pciutils
  ];

  system.stateVersion = "23.05";
}
