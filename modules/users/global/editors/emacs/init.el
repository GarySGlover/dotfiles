;; -*- lexical-binding: t -*-

(let ((emacs-init-file (expand-file-name "emacs-config.el" "~/.config/emacs"))
      (emacs-org-file (expand-file-name "emacs-config.org" "~/.config/emacs")))
  (delete-file emacs-init-file)
  (org-babel-load-file emacs-org-file))
