# [[file:../modules.org::*Kubernetes][Kubernetes:1]]
{
  flake.aspects = {
    options = {
      homeManager =
        { config, lib, ... }:
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
              home.packages = with pkgs; [
                kubectl
                kubernetes-helm
                k9s
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
