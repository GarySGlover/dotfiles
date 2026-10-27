# [[file:../../modules.org::*Mode/Header line][Mode/Header line:2]]
{
  flake.aspects.editor.homeManager = {
    editor.initFiles.modeline.text = ''
      (setopt mode-line-collapse-minor-modes
              '(abbrev-mode
                auto-revert-mode
                buffer-face-mode
                dtrt-indent-mode
                editorconfig-mode
                eldoc-mode
                meow-beacon-mode
                meow-insert-mode
                meow-keypad-mode
                meow-motion-mode
                meow-normal-mode
                org-src-mode
                selected-minor-mode
                single-window-mode
                visual-line-mode
                whitespace-mode))
      (use-package telephone-line
        :config
        (telephone-line-defsegment* my-telephone-line-minor-mode-segment ()
          (mode-line--minor-modes))
        (defun telephone-line-modal-face (active)
          (cond ((not active) 'mode-line-inactive)
                ((and meow-normal-mode (region-active-p)) 'telephone-line-evil-visual)
                (meow-normal-mode 'telephone-line-evil-normal)
                (meow-insert-mode 'telephone-line-evil-insert)
                (meow-motion-mode 'telephone-line-evil-emacs)
                (meow-keypad-mode 'telephone-line-evil-operator)
                (meow-beacon-mode 'telephone-line-evil-replace)))
        (setq telephone-line-lhs
              '((evil . (telephone-line-meow-tag-segment))
                (accent . (telephone-line-vc-segment
                           telephone-line-process-segment))
                (nil . (telephone-line-buffer-name-segment
                        my-telephone-line-minor-mode-segment))))
        (setq telephone-line-rhs
              '((nil . ())
                (accent . (telephone-line-misc-info-segment))
                (evil . (telephone-line-major-mode-segment))))
        :hook (after-init . telephone-line-mode))
      (setopt
       display-time-format "%H:%M"
       display-time-default-load-average nil)
      (display-time-mode)
    '';
    programs.emacs.extraPackages =
      epkgs: with epkgs; [
        breadcrumb
        telephone-line
      ];
  };
}
# Mode/Header line:2 ends here
