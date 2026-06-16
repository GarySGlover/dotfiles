# [[file:../../modules.org::*Presentation][Presentation:1]]
{
  flake.aspects.editor.homeManager = {
    programs.emacs.extraPackages =
      epkgs: with epkgs; [
        org-tree-slide
      ];
  };
}
# Presentation:1 ends here
