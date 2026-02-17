# [[file:../modules.org::*Audio][Audio:1]]
{ inputs, lib, ... }:
{
  flake.aspects =
    { aspects, ... }:
    {
      audio = {
        homeManager =
          { pkgs, ... }:
          {
            home.packages = with pkgs; [
              bluetuith
            ];
          };

        nixos = {
          services.pulseaudio.enable = false;
          services.pipewire = {
            enable = true;
            alsa.enable = true;
            alsa.support32Bit = true;
            pulse.enable = true;
          };
        };
      };
    };
}
# Audio:1 ends here
