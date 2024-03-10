{
  config,
  lib,
  pkgs,
  ...
}:
with lib; {
  config = mkIf config.wolf.roles.gaming {
    home.packages = with pkgs; [
      mangohud
    ];
  };
}
