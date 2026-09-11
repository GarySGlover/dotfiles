# [[file:../modules.org::*Help][Help:2]]
{
  flake.aspects = {
    notes.homeManager = {
      editor.initFiles.help.text = ''
        ;; Embark for Emacs help
        (setq prefix-help-command #'embark-prefix-help-command)
        (bind-key "C-h C-h" #'embark-prefix-help-command help-quick-use-map)
        (bind-key "C-h b" #'embark-bindings)

        ;; Man will use pop-to-buffer which respects the display-buffer rules
        (setopt Man-notify-method 'aggressive)
      '';
    };
  };
}
# Help:2 ends here
