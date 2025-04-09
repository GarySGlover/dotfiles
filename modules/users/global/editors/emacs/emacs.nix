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
      package = pkgs.emacs;
    };
    services.emacs.enable = true;
    home.sessionVariables.EDITOR = "${pkgs.writeShellScript "emacs-editor" ''
      #!/usr/bin/env bash
      if infocmp xterm | grep -q 'xterm' &> /dev/null; then
          emacsclient --tty --alternate-editor "" "$@"
      else
          emacsclient --reuse-frame --alternate-editor "" "$@"
      fi
    ''}";

    xdg.configFile."emacs/emacs-config.el".source = ./emacs-config.el;
    xdg.configFile."emacs/early-init.el".text = ''
      ;; -*- lexical-binding: t -*-

      (let* ((cache-dir (expand-file-name "eln-cache/" user-emacs-directory))
             (version-dir (car (directory-files cache-dir t (concat "^" (regexp-quote emacs-version) "-"))))
             (vertico-dir (and version-dir (directory-files version-dir t "^vertico-"))))
        (when vertico-dir
          (dolist (dir vertico-dir)
            (delete-file dir))))

      (setopt gc-cons-threshold (* 50 1000 1000))
    '';
    xdg.configFile."emacs/init.el".text =
      ''
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
