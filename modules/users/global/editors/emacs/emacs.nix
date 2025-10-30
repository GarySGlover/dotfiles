{
  pkgs,
  config,
  lib,
  ...
}:
with lib;
let
  theme = config.wolf.theme;
  secrets = import "${config.wolf.secretsPath}/${config.home.username}-secrets.nix";
in
{
  config = mkIf config.wolf.roles.editing {
    programs.emacs = {
      enable = true;
      package = pkgs.emacs-git-pgtk;
    };
    services.emacs.enable = false;

    xdg.configFile."emacs/early-init.el".source = ./emacs-new/early-init.el;
    xdg.configFile."emacs/init.el".source = ./emacs-new/init.el;
    xdg.configFile."emacs/cnit-functions.el".source = ./emacs-new/cnit-functions.el;
    xdg.configFile."emacs/external-programs.el".text = ''
      ;;; external-programs.el --- Paths to external programs required by my emacs configuration and custom functions.  -*- lexical-binding: t; -*-

      ;; Copyright (C) 2025

      ;; Author:  <>
      ;; Keywords: local

      ;; This program is free software; you can redistribute it and/or modify
      ;; it under the terms of the GNU General Public License as published by
      ;; the Free Software Foundation, either version 3 of the License, or
      ;; (at your option) any later version.

      ;; This program is distributed in the hope that it will be useful,
      ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
      ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
      ;; GNU General Public License for more details.

      ;; You should have received a copy of the GNU General Public License
      ;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

      ;;; Commentary:

      ;;

      ;;; Code:
      (defvar cnit-gsettings-executable "${pkgs.glib}/bin/gsettings"
        "Path to the gsettings executable.")

      (provide 'external-programs)
      ;;; external-programs.el ends here
    '';

    home.sessionVariables.EDITOR = "${pkgs.writeShellScript "emacs-editor" ''
      #!/usr/bin/env bash
      if infocmp xterm | grep -q 'xterm' &> /dev/null; then
          emacsclient --tty --alternate-editor "" "$@"
      else
          emacsclient --reuse-frame --alternate-editor "" "emacs"
      fi
    ''}";

    programs.git.ignores = [
      "*~"
      ".#*"
      "*.elc"
      "*.tmp"
      "*#"
      ".aider*"
    ];
  };
}
