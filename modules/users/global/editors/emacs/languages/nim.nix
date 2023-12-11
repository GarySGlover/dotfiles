{
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  opt = config.wolf;
in {
  config = mkIf (opt.roles.programming
    && opt.languages.nim) {
    programs.emacs.extraPackages = epkgs:
      with epkgs; [
        nim-mode
      ];

    home.sessionVariables = {WOLF_LANGUAGE_NIM = "true";};

    programs.emacs.extraConfig = ''
      (exec-path-from-shell-copy-env "WOLF_LANGUAGE_NIM")
    '';
  };
}
