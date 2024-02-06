{
  config,
  lib,
  pkgs,
  ...
}:
with lib; {
  config = mkIf config.wolf.roles.cad {
    home.packages = with pkgs; [
      prusa-slicer
    ];
  };
}
