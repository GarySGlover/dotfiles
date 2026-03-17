{
  pkgs,
  lib,
  ...
}:
with lib;
{
  config = {
    home.packages = with pkgs; [
      nodePackages.bash-language-server
      yaml-language-server
    ];
  };
}
