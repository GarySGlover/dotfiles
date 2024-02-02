{
  config,
  pkgs,
  lib,
  ...
}:
with lib; {
  config = mkIf config.wolf.roles.programming {
    home.packages = with pkgs; [
      yaml-language-server
    ];
  };
}
