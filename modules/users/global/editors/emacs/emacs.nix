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
      extraPackages = epkgs:
        with epkgs; [
          doom-themes
          exec-path-from-shell
          elisp-autofmt
          forge
          format-all
          magit
          no-littering
          json-mode
          json-navigator
        ];
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

    home.file.authinfo = {
      target = ".authinfo";
      text =
        if (hasAttr "github_user" secrets)
        then ''
          machine api.github.com login ${secrets.github_user}^forge password ${secrets.github_magit_forge_token}
        ''
        else "";
    };
  };
}
