{
  config,
  lib,
  pkgs,
  ...
}:
with lib; {
  config = mkIf config.wolf.roles.programming {
    home.packages = with pkgs; [
      qmk
    ];
  };
}
