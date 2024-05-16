{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkIf;
in {
  config = mkIf config.wolf.roles.devops {
    home.packages = with pkgs; [
      azure-cli
      kubelogin
    ];
  };
}
