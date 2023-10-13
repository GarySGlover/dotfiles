{
  pkgs,
  config,
  lib,
  ...
}:
with lib; {
  config = mkIf (config.wolf.editors.emacs.enable
    && config.wolf.languages.nix.enable) {
    programs.emacs.extraPackages = epkgs:
      with epkgs; [
        nix-mode
      ];

    home.sessionVariables = {WOLF_LANGUAGE_NIX = "true";};

    programs.emacs.extraConfig = ''
      (exec-path-from-shell-copy-env "WOLF_LANGUAGE_NIX")
    '';

    home.packages = with pkgs; [
      rnix-lsp
    ];
  };
}
