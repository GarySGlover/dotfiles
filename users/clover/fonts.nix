{ pkgs, ... }:

{
  # Needed to pickup fonts installed by home manager
  fonts.fontconfig.enable = true;

  home.packages = with pkgs; [
    fira-code
    (nerdfonts.override { fonts = [ "FiraCode" ]; })
  ];
}
