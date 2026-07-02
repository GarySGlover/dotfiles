# [[file:../../modules.org::*Presentation][Presentation:2]]
{
  flake.aspects.editor.homeManager = {
    editor.initFiles.presentation.text = ''
      (with-eval-after-load 'org-tree-slide
        (bind-keys
         :map org-tree-slide-mode-map
         ("," . org-tree-slide-move-previous-tree)
         ("." . org-tree-slide-move-next-tree)))
    '';
    programs.emacs.extraPackages =
      epkgs: with epkgs; [
        org-tree-slide
      ];
  };
}
# Presentation:2 ends here
