{
  config,
  pkgs,
  lib,
  ...
}:
with lib; {
  config = mkIf config.wolf.roles.programming {
    home.packages = with pkgs; [
      nodePackages.bash-language-server
      yaml-language-server
    ];
  };
}
