{
  config,
  pkgs,
  lib,
  ...
}:
let
  inherit (lib) mkIf;
  roles = config.wolf.roles;
in
{
  config = mkIf (roles.editing || roles.desktop) {
    # Needed to pickup fonts installed by home manager
    fonts.fontconfig.enable = true;

    home.packages = with pkgs; [
      nerd-fonts._0xproto
    ];
  };
}
