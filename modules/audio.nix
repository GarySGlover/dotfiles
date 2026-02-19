# [[file:../modules.org::*Audio][Audio:1]]
{
  flake.aspects = {
    audio = {
      homeManager = {
        programs.bluetuith.enable = true;
        niri.configFiles.audio_binds.text = ''
          binds {
          	XF86AudioLowerVolume allow-when-locked=true {
          		spawn "swayosd-client" "--output-volume=lower"
          	}
          	XF86AudioRaiseVolume allow-when-locked=true {
          		spawn "swayosd-client" "--output-volume=raise"
          	}
          	XF86AudioMute allow-when-locked=true {
          		spawn "swayosd-client" "--output-volume=mute-toggle"
          	}
          	XF86AudioMicMute allow-when-locked=true {
          		spawn "swayosd-client" "--input-volume=mute-toggle"
          	}
          }
        '';
        services.swayosd.enable = true;
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
