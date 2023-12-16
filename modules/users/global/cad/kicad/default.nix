{
  config,
  lib,
  pkgs,
  ...
}:
with lib; {
  config = mkIf config.wolf.roles.electrical {
    home.packages = with pkgs; [
      kicad
    ];
  };
}
