{
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  opt = config.wolf;
in {
  config = mkIf (opt.roles.editing
    && opt.roles.programming
    && opt.languages.nim) {
    programs.emacs.extraPackages = epkgs:
      with epkgs; [
        nim-mode
      ];
  };
}
