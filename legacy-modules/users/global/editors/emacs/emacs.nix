{
  pkgs,
  ...
}:
let
  az = pkgs.azure-cli.withExtensions (
    with pkgs.azure-cli-extensions;
    [
      aks-preview
      azure-devops
      interactive
      subscription
    ]
  );
in
{
  config = {
    editor.earlyInitFiles = {
      external-programs = {
        priority = 5;
        text = ''
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
          (defvar cnit-az-executable "${az}/bin/az"
            "Path to the azure cli executable.")

          (provide 'external-programs)
          ;;; external-programs.el ends here
        '';
      };

      cnit-functions = {
        text = (builtins.readFile ./emacs-new/cnit-functions.el);
        priority = 10;
      };
      early = {
        text = (builtins.readFile ./emacs-new/early-init.el);
        priority = 20;
      };
    };

    editor.initFiles = {
      legacy = {
        text = (builtins.readFile ./emacs-new/init.el);
        priority = 10;
      };
    };

    xdg.configFile."emacs/templates".source = ./emacs-new/templates;
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
