{ pkgs, ...}:

{
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
    options = "--delete-older-than 7d";
  };

  # Set your time zone.
  time.timeZone = "Europe/London";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_GB.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "en";
  };

  # Force users to be configured by nix
  users.mutableUsers = false;

  system.stateVersion = "23.05";
}
