{
  config,
  lib,
  ...
}:
with lib;
{
  config = mkIf config.wolf.system.physical {
    programs.niri.enable = true;

    services.xserver = {
      enable = true;
      # Configure keymap in X11
      xkb.layout = "gb";
      videoDrivers = [ "modesetting" ];
      displayManager.startx.enable = true;
    };

    environment.etc."X11/xinit/xinitrc".text = "";

    # Enable sound.
    services.pulseaudio.enable = false;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };
  };
}
