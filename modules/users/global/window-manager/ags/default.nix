{
  pkgs,
  config,
  lib,
  ...
}:
with lib; {
  config = mkIf config.wolf.roles.desktop {
    home.packages = with pkgs; [
      ags
    ];
  };
}
