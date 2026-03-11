# [[file:../../modules.org::*Global Minor Modes][Global Minor Modes:2]]
{
  flake.aspects.editor.homeManager = {
    editor.initFiles.global-minor-modes.text = ''
      (add-hook 'after-init-hook
                (lambda ()
                  (popper-mode t)
                  (winner-mode t)
                  (repeat-mode t)
                  (delete-selection-mode 1)
                  (vertico-mode t)
                  (vertico-multiform-mode t)
                  (marginalia-mode)
                  (editorconfig-mode t)))
    '';
  };
}
# Global Minor Modes:2 ends here
