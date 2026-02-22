# [[file:../../modules.org::*+niri][+niri:2]]
{
  flake.aspects.editor.homeManager = {
    niri.configFiles.audio = {
      text = ''
        spawn-sh-at-startup "systemctl --user restart swayosd"
      '';
      priority = 2000;
    };
  };
}
# +niri:2 ends here
