{
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  opt = config.wolf;
in {
  config = mkIf opt.roles.editing {
    programs.emacs.extraPackages = epkgs:
      with epkgs; [
        elisp-autofmt
        flycheck
        flycheck-eglot
        json-mode
        json-navigator
        markdown-mode
        nim-mode
        nix-ts-mode
        terraform-doc
        terraform-mode
      ];
  };
}
