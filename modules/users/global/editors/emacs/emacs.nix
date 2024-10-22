{
  pkgs,
  config,
  lib,
  ...
}:
let
  inherit (lib) mkIf;
in
{
  config = mkIf config.wolf.roles.editing {
    programs.emacs = {
      enable = true;
      package = pkgs.emacs29-pgtk;
    };
    home.sessionVariables.EDITOR = "${pkgs.writeShellScript "emacs-editor" ''
      #!/usr/bin/env bash
      emacsclient -c -a emacs $@
    ''}";

    xdg.configFile."emacs/emacs-config.org".source = ./emacs-config.org;
    xdg.configFile."emacs/early-init.el".source = ./early-init.el;
    xdg.configFile."emacs/init.el".source = ./init.el;
    home.packages = with pkgs; [
      yamlfmt
      yamllint
    ];
    programs.git.ignores = [
      "*~"
      ".#*"
      "*.elc"
      "*.tmp"
      "*#"
    ];
  };
}
