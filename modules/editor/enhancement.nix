# [[file:../../modules.org::*Editing enhancement][Editing enhancement:2]]
{
  flake.aspects.editor.homeManager = {
    editor.initFiles.editingEnhancement.text = ''
      (with-eval-after-load 'selected
        (setopt
         insert-pair-alist
         '((?\( ?\)) (?\[ ?\]) (?\{ ?\}) (?\< ?\>) (?\" ?\") (?\' ?\') (?\` ?\') (?\= ?\=) (?\~ ?\~)))

        (bind-keys
         :map selected-keymap
         ("(" . insert-pair)
         ("[" . insert-pair)
         ("{" . insert-pair)
         ("<" . insert-pair)
         ("\"" . insert-pair)
         ("'" . insert-pair)
         ("`" . insert-pair)
         ("=" . insert-pair)
         ("~" . insert-pair)
         ("R" . replace-regexp)
         ("d" . downcase-dwim)
         ("l" . sort-lines)
         ("r" . replace-string)
         ("u" . upcase-dwim)
         ("x" . exchange-point-and-mark)))
    '';
    programs.emacs.extraPackages =
      epkgs: with epkgs; [
        selected
      ];
  };
}
# Editing enhancement:2 ends here
