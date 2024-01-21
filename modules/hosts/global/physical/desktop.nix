{
  config,
  lib,
  ...
}:
with lib; {
  config = mkIf config.wolf.system.physical {
    programs.hyprland = {
      enable = true;
      xwayland.enable = true;
    };

    services.xserver = {
      enable = true;
      displayManager.sddm = {enable = true;};
      displayManager.defaultSession = "hyprland";
    };

    # Configure keymap in X11
    services.xserver.layout = "gb";

    # Enable sound.
    hardware.pulseaudio.enable = false;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };
  };
}
