{
  config,
  lib,
  pkgs,
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

    # Fix webcam frame rate
    services.udev.extraRules = ''
      SUBSYSTEM=="video4linux", KERNEL=="video[0-9]*", ATTR{name}=="*camera*", RUN+="${pkgs.v4l-utils}/bin/v4l2-ctl --device=/dev/%k --set-ctrl=power_line_frequency=1"
      SUBSYSTEM=="video4linux", KERNEL=="video[0-9]*", ATTR{name}=="*webcam*", RUN+="${pkgs.v4l-utils}/bin/v4l2-ctl --device=/dev/%k --set-ctrl=power_line_frequency=1"
      SUBSYSTEM=="video4linux", KERNEL=="video[0-9]*", RUN+="${pkgs.v4l-utils}/bin/v4l2-ctl --device=/dev/%k --set-ctrl=power_line_frequency=1"
    '';
  };
}
