# [[file:../modules.org::*Help][Help:2]]
{
  flake.aspects = {
    notes.homeManager = {
      editor.initFiles.help.text = ''
        ;; Embark for Emacs help
        (setq prefix-help-command #'embark-prefix-help-command)
        (bind-key "C-h C-h" #'embark-prefix-help-command help-quick-use-map)
        (bind-key "C-h b" #'embark-bindings)

        ;; Help type buffer display rules
        (add-to-list 'display-buffer-alist
                     `(,(rx (or
                             (and "*" (or "H" "h") "elp")
                             "*info"
                             "*Man"
                             "*Summary"
                             "*Apropos"))
                       (display-buffer-reuse-window
                        display-buffer-reuse-mode-window)
                       (mode apropros-mode help-mode helpful-mode Info-mode Man-mode)
                       (reusable-frames . nil)))
        ;; Man will use pop-to-buffer which respects the display-buffer rules
        (setopt Man-notify-method 'aggressive)
      '';
    };
  };
}
# Help:2 ends here
