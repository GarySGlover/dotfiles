{
  pkgs,
  config,
  lib,
  ...
}: let
  inherit (lib) mkIf;
in {
  config = mkIf config.wolf.roles.editing {
    programs.emacs = {
      enable = true;
      package = pkgs.emacs29-pgtk;
      extraConfig = ''
        (delete-file (expand-file-name "emacs-config.el" "~/.config/emacs"))
        (org-babel-load-file (expand-file-name "emacs-config.org" "~/.config/emacs"))
      '';
    };
    services.emacs = {
      enable = true;
      defaultEditor = true;
    };

    xdg.configFile."emacs/emacs-config.org".source = ./emacs-config.org;

    home.packages = with pkgs; [
      alejandra
      nixd
      yamlfmt
      yamllint
    ];
  };
}
