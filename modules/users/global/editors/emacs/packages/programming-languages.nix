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
        json-mode
        json-navigator
        nim-mode
        nix-ts-mode
        markdown-mode
        terraform-doc
        terraform-mode
      ];
  };
}
