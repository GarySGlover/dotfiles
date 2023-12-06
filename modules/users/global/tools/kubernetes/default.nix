{
  config,
  lib,
  pkgs,
  ...
}:
with lib; {
  config = mkIf config.wolf.roles.devops {
    home.packages = with pkgs; [
      kubectl
      kubernetes-helm
      openlens
    ];
  };
}
