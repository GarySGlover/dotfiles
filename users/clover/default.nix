{ config, pkgs, ... }:

{
  sops.secrets.clover-password.neededForUsers = true;

  users.users.clover = {
    isNormalUser = true;
    home = "/home/clover";
    uid = 1000;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    passwordFile = config.sops.secrets.clover-password.path;
    packages = with pkgs; [
       firefox
       emacs
       emacsPackages.nix-mode
       git pre-commit
       go python311
       wlr-randr
       alacritty
       age sops ssh-to-age # Nix-sops secrets management
     ];
  };
}
