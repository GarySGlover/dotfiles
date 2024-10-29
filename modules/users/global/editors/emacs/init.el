;; -*- lexical-binding: t -*-

(let ((emacs-init-file (expand-file-name "emacs-config.el" "~/.config/emacs"))
      (emacs-org-file (expand-file-name "emacs-config.org" "~/.config/emacs")))
  (delete-file emacs-init-file)
  (org-babel-load-file emacs-org-file))

(use-package ef-themes
  :init
  (mapc #'disable-theme custom-enabled-themes)
  (ef-themes-select 'ef-elea-dark))

;; Set font and font size
(defun cloveynit-after-frame ()
  (set-face-attribute 'default nil :family "FiraCode Nerd Font" :height 120)
  (set-face-attribute 'fixed-pitch nil :family "FiraCode Nerd Font" :height 120)
  (set-face-attribute 'fixed-pitch-serif nil :family "FiraCode Nerd Font" :height 120)
  (set-face-attribute 'variable-pitch nil :family "DaddyTimeMono Nerd Font Propo" :height 120))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'cloveynit-after-frame)
  (cloveynit-after-frame))
