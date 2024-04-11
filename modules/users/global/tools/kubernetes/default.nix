{
  config,
  lib,
  pkgs,
  ...
}:
with lib; {
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
