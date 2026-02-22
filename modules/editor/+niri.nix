# [[file:../../modules.org::*+niri][+niri:2]]
{
  flake.aspects.editor.homeManager = {
    niri.configFiles.editor = {
      text = ''
        spawn-sh-at-startup "systemctl --user restart emacs"
      '';
      priority = 2000;
    };
  };
}
# +niri:2 ends here
