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
        markdown-mode
        (callPackage ./manual/nim-ts-mode.nix {
          inherit (pkgs) fetchFromGitHub writeText;
          inherit (epkgs) melpaBuild nim-mode;
        })
        nix-ts-mode
        verb
        terraform-doc
        terraform-mode
      ];
  };
}
