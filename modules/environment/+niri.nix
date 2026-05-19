# [[file:../../modules.org::*+niri][+niri:2]]
{
  flake.aspects.environment.homeManager =
    { config, ... }:
    {
      niri.configFiles.environment.text = ''
        environment {
          DISPLAY ":0"
          XDG_CONFIG_HOME "${config.xdg.configHome}"
        }
      '';
    };
}
# +niri:2 ends here
