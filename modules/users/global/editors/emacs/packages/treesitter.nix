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
        treesit-grammars.with-all-grammars
      ];
    home.file.".config/emacs/var/tree-sitter".source = "${pkgs.emacsPackages.treesit-grammars.with-all-grammars}/lib";
  };
}
