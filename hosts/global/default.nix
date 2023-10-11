{pkgs, ...}: {
  imports = [
    ./users.nix
  ];

  # Default sops config
  sops.defaultSopsFile = ../../secrets/global.yaml;

  # Nix flakes
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  # Nix Garbage Collection
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 31d";
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
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Extra system packages
  environment.systemPackages = with pkgs; [
    pciutils
  ];

  # https://github.com/NixOS/nixpkgs/issues/162562

  system.stateVersion = "23.05";
}
