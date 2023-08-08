{ config, pkgs, ... }:

{
  sops.secrets.clover-password.neededForUsers = true;

  users.users.clover = {
    isNormalUser = true;
    home = "/home/clover";
    uid = 1000;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    passwordFile = config.sops.secrets.clover-password.path;
  };
}
