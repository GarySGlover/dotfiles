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

    xdg.configFile."emacs-new/early-init.el".source = ./emacs-new/early-init.el;
    xdg.configFile."emacs-new/init.el".source = ./emacs-new/init.el;
    xdg.configFile."emacs-new/cnit-functions.el".source = ./emacs-new/cnit-functions.el;
    xdg.configFile."emacs-new/external-programs.el".text = ''
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

    xdg.configFile."emacs/emacs-config.el".source = ./emacs-config.el;
    xdg.configFile."emacs/early-init.el".text = ''
      ;; -*- lexical-binding: t -*-

      (setq use-package-compute-statistics t)

      (setopt gc-cons-threshold (* 50 1000 1000))
    '';
    xdg.configFile."emacs/init.el".text = ''
      ;; -*- lexical-binding: t -*-

      (let ((emacs-init-file (expand-file-name "emacs-config.el" "~/.config/emacs")))
        (load-file emacs-init-file))

      (use-package ef-themes
        :init
        (mapc #'disable-theme custom-enabled-themes)
        (ef-themes-select '${theme.name}))

      ;; Set font and font size
      (defun cloveynit-after-frame ()
        (set-face-attribute 'default nil :family "${theme.font.name}" :height ${toString theme.font.size}0)
        (set-face-attribute 'fixed-pitch nil :family "${theme.font.name}" :height ${toString theme.font.size}0)
        (set-face-attribute 'fixed-pitch-serif nil :family "${theme.font.name}" :height ${toString theme.font.size}0)
        (set-face-attribute 'variable-pitch nil :family "${theme.font.name}" :height ${toString theme.font.size}0)
        (let ((error-foreground (face-foreground 'error))
              (error-background (face-background 'error)))
          (set-face-foreground 'font-lock-comment-face error-foreground)
          (set-face-background 'font-lock-comment-face error-background)))

      (if (daemonp)
          (add-hook 'server-after-make-frame-hook #'cloveynit-after-frame)
        (cloveynit-after-frame))

      (setq ispell-alternate-dictionary (expand-file-name "emacs/dict.txt" (xdg-config-home)))
    ''
    + (
      if (hasAttr "copilot_enabled_organisations" secrets) then
        ''
          (defvar cnit/copilot-enabled-organisations '(${lib.strings.concatStringsSep " " secrets.copilot_enabled_organisations}))
        ''
      else
        ""
    );
    programs.git.ignores = [
      "*~"
      ".#*"
      "*.elc"
      "*.tmp"
      "*#"
    ];
  };
}
