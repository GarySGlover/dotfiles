{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  opt = config.wolf.tools.kubernetes;
in {
  options.wolf.tools.kubernetes.enable = mkOption {
    type = types.bool;
  };

  config = mkIf opt.enable {
    home.packages = with pkgs; [
      kubectl
      kubernetes-helm
      openlens
    ];
  };
}
