# [[file:../../modules.org::*Editor][Editor:1]]
{ ... }:
{
  flake.aspects =
    { aspects, ... }:
    {
      config = {
        editor = {
          includes = with aspects; [ ];
          homeManager =
            { pkgs, ... }:
            {
              config = {
                programs.emacs = {
                  enable = true;
                  # The nix-wrapper-modules Emacs wrapper does not carry
                  # `nix-support/setup-hook` from the Emacs it wraps, but that hook
                  # (`addEmacsVars`, registered through `addEnvHooks`) is what puts a
                  # package's dependencies on `EMACSLOADPATH` while byte-compiling it.
                  # Without it, any package built from this Emacs' package set fails to
                  # find its `packageRequires` — e.g. stylix's generated
                  # `base16-stylix-theme`, which `(require 'base16-theme)`s and died
                  # with "Cannot open load file: No such file or directory, base16-theme".
                  # Re-wrap to graft the hook back on from stock Emacs.
                  package =
                    (pkgs.mkEmacsWith [
                      {
                        init = builtins.readFile ../../legacy-modules/users/global/editors/emacs/local.el;
                        # `local.el' `require's these itself: dash provides the
                        # `-when-let*'/`-each'/`-sort' macros used by the `gitr-*'
                        # helpers, consult the completion API used by the Azure
                        # subscription/cluster pickers.
                        packages = with pkgs.emacsPackages; [
                          consult
                          dash
                        ];
                      }
                    ]).wrap
                      {
                        drv.postInstall = ''
                          mkdir -p $out/nix-support
                          cp ${pkgs.emacs}/nix-support/setup-hook $out/nix-support/setup-hook
                        '';
                      };
                };
                services.emacs = {
                  enable = true;
                  defaultEditor = true;
                  startWithUserSession = "graphical";
                };
              };
            };
        };
      };
    };
}
# Editor:1 ends here
