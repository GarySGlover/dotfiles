# [[file:../../modules.org::*Casual][Casual:2]]
{
  flake.aspects.editor.homeManager = {
    editor.initFiles.casual.text = ''
      (keymap-global-set "C-o" #'casual-editkit-main-tmenu)
    '';
  };
}
# Casual:2 ends here
