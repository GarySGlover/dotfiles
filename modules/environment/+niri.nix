# [[file:../../modules.org::*+niri][+niri:2]]
{
  flake.aspects.environment.homeManager = {
    niri.configFiles.environment.text = ''
      environment {
        DISPLAY ":0"
        XDG_CONFIG_HOME "/home/clover/.config"
      }
    '';
  };
}
# +niri:2 ends here
