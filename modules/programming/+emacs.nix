# [[file:../../modules.org::*+emacs][+emacs:1]]
{
  flake.aspects.programming.homeManager = {
    programs.emacs.extraPackages =
      epkgs: with epkgs; [
        reformatter
      ];
  };
}
# +emacs:1 ends here
