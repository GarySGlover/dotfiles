# [[file:../../modules.org::*Global Minor Modes][Global Minor Modes:2]]
{
  flake.aspects.editor.homeManager = {
    editor.initFiles.global-minor-modes.text = ''
      (add-hook 'after-init-hook
                (lambda ()
                  (when (fboundp 'popper-mode) (popper-mode t))
                  (when (fboundp 'winner-mode) (winner-mode t))
                  (when (fboundp 'repeat-mode) (repeat-mode t))
                  (when (fboundp 'delete-selection-mode) (delete-selection-mode 1))
                  (when (fboundp 'vertico-mode) (vertico-mode t))
                  (when (fboundp 'vertico-multiform-mode) (vertico-multiform-mode t))
                  (when (fboundp 'marginalia-mode) (marginalia-mode))
                  (when (fboundp 'editorconfig-mode) (editorconfig-mode t))
                  (when (fboundp 'dtrt-indent-global-mode) (dtrt-indent-global-mode t))
                  (when (fboundp 'selected-global-mode) (selected-global-mode t))
                  (when (fboundp 'breadcrumb-mode) (breadcrumb-mode t)))
                  (when (fboundp 'single-window-mode) (single-window-mode t)))
    '';
  };
}
# Global Minor Modes:2 ends here
