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
    && opt.languages.nix) {
    programs.emacs.extraPackages = epkgs:
      with epkgs; [
        nix-mode
      ];

    home.sessionVariables = {WOLF_LANGUAGE_NIX = "true";};

    programs.emacs.extraConfig = ''
      (exec-path-from-shell-copy-env "WOLF_LANGUAGE_NIX")
    '';

    home.packages = with pkgs; [
      alejandra
      rnix-lsp
    ];
  };
}
