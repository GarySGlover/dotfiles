# [[file:../modules.org::*Kubernetes][Kubernetes:1]]
{
  flake.aspects = {
    options = {
      homeManager =
        { lib, ... }:
        {
          options.kubernetes.includeKubelogin = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = "Enable kubelogin for kubectl";
          };
        };
    };
    kubernetes = {
      homeManager =
        {
          pkgs,
          config,
          lib,
          ...
        }:
        {
          config = lib.mkMerge [
            {
              programs.k9s.enable = true;
              home.packages = with pkgs; [
                kubectl
                kubernetes-helm
              ];
            }
            (lib.mkIf config.kubernetes.includeKubelogin {
              home.packages = with pkgs; [ kubelogin ];
            })
          ];
        };
    };
  };
}
# Kubernetes:1 ends here
