# [[file:../../modules.org::*Navigation][Navigation:2]]
{
  flake.aspects.editor.homeManager = {
    editor.initFiles.navigation.text = ''
      (bind-key "M-j" #'avy-goto-char-timer)
      (bind-key "M-j" #'avy-isearch isearch-mode-map)
      (with-eval-after-load 'avy
        (setopt avy-style 'at-full
                avy-single-candidate-jump nil
                avy-all-windows-alt 'all-frames))
    '';
    programs.emacs.extraPackages =
      epkgs: with epkgs; [
        avy
      ];
  };
}
# Navigation:2 ends here
