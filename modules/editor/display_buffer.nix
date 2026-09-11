# [[file:../../modules.org::*Display buffer defualts][Display buffer defualts:2]]
{
  flake.aspects.editor.homeManager = { inputs, pkgs, ... }: {
    editor.initFiles.display_buffer.text = ''
      (with-eval-after-load 'single-window
        (setopt
         single-window-respect-display-buffer-alist t
         single-window-exclude-popper t))
    '';
    programs.emacs.extraPackages = _: [
      (inputs.self.packages.${pkgs.stdenv.hostPlatform.system}.emacsSingleWindow)
    ];
  };
  perSystem =
    { pkgs, ... }:
    {
      packages.emacsSingleWindow = pkgs.emacsPackages.melpaBuild {
        pname = "single-window";
        version = "1.0.1";
        src = pkgs.fetchFromGitHub {
          owner = "jamescherti";
          repo = "single-window.el";
          rev = "38483532db9c1e7a2c07a04dd9f70f9b288bca7c";
          hash = "sha256-ZsIiCu2pzuJBv4rYRD45loI4bjJBMpnAE7ZuqXXZvU4=";
        };
      };
    };
}
# Display buffer defualts:2 ends here
