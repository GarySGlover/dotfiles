{
  config,
  pkgs,
  lib,
  ...
}:
with lib; let
  roles = config.wolf.roles;
in {
  config = mkIf (roles.editing || roles.desktop) {
    # Needed to pickup fonts installed by home manager
    fonts.fontconfig.enable = true;

    home.packages = with pkgs; [
      fira-code
      fira-code-symbols
      (nerdfonts.override {fonts = ["FiraCode"];})
    ];
  };
}
