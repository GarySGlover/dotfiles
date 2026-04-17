# [[file:../../modules.org::*Casual][Casual:2]]
{
  flake.aspects.editor.homeManager = {
    editor.initFiles.casual.text = ''
      (keymap-global-set "C-o" #'casual-editkit-main-tmenu)
    '';
    programs.emacs.extraPackages =
      epkgs: with epkgs; [
        casual
      ];
  };
}
# Casual:2 ends here
