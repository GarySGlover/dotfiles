# [[file:../modules.org::*Kubernetes][Kubernetes:2]]
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
                helm-dashboard
              ];
              editor.initFiles.kubernetes.text = ''
                (add-hook
                 'after-init-hook
                 (lambda ()
                   (ghostel-make-exec k9s)))
              '';
              programs.emacs.extraPackages =
                epkgs: with epkgs; [
                  kubel
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
# Kubernetes:2 ends here
