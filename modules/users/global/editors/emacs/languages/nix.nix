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
    && opt.languages.nix) {
    programs.emacs.extraPackages = epkgs:
      with epkgs; [
        nix-ts-mode
      ];

    home.packages = with pkgs; [
      alejandra
      rnix-lsp
    ];
  };
}
