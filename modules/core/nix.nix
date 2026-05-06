# [[file:../../modules.org::*Nix][Nix:1]]
{
  flake.aspects.core = {
    homeManager =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [
          deadnix
          manix
          nh
          nix-index
          nix-tree
          nixd
          nixfmt
        ];
        programs.emacs.extraPackages =
          epkgs: with epkgs; [
            nix-ts-mode
          ];
        editor.initFiles.nix.text = ''
          (with-eval-after-load 'nix-ts-mode
            (require 'reformatter)
            (require 'editorconfig)
            (require 'dtrt-indent)
            (reformatter-define nix-format
              :program "nixfmt")
            (defun nix-ts-mode-setup ()
              (nix-format-on-save-mode 1))
            (add-hook 'nix-ts-mode-hook #'nix-ts-mode-setup))
        '';
      };
    nixos = { };
  };
}
# Nix:1 ends here
