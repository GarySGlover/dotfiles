{
  pkgs,
  config,
  lib,
  ...
}:
with lib;
with builtins; let
  secrets = import "${config.wolf.secretsPath}/${config.home.username}-secrets.nix";
in {
  config = mkIf config.wolf.roles.editing {
    programs.emacs = {
      enable = true;
      package = pkgs.emacs29-gtk3;
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

    # Hide emacs from application menu
    xdg.desktopEntries.emacs = {
      name = "Emacs";
      noDisplay = true;
    };

    home.packages = with pkgs; [
      alejandra
      rnix-lsp
      yamlfmt
      yamllint
    ];
  };
}
