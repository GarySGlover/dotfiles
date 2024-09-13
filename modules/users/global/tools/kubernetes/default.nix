{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkIf;
in
{
  config = mkIf config.wolf.roles.devops {
    home.packages = with pkgs; [
      argo-rollouts
      helm-dashboard
      kubectl
      kubernetes-helm
    ];

    programs.k9s = {
      enable = true;
    };
  };
}
