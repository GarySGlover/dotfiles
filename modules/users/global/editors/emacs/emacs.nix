{
  pkgs,
  config,
  lib,
  ...
}:
with lib;
let
  theme = config.wolf.theme;
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

    xdg.configFile."emacs/emacs-config.el".source = ./emacs-config.el;
    xdg.configFile."emacs/early-init.el".text = ''
      ;; -*- lexical-binding: t -*-

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
        (set-face-attribute 'variable-pitch nil :family "${theme.font.name}" :height ${toString theme.font.size}0))

      (if (daemonp)
          (add-hook 'server-after-make-frame-hook #'cloveynit-after-frame)
        (cloveynit-after-frame))
    '';
    programs.git.ignores = [
      "*~"
      ".#*"
      "*.elc"
      "*.tmp"
      "*#"
    ];
  };
}
