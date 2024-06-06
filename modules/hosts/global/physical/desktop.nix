{
  config,
  lib,
  ...
}: let
  inherit (lib) mkIf;
in {
  config = mkIf config.wolf.system.physical {
    programs.hyprland = {
      enable = true;
      xwayland.enable = true;
    };

    programs.sway = {
      enable = true;
      wrapperFeatures.gtk = true;
    };

    services.xserver = {
      enable = true;
      # Configure keymap in X11
      xkb.layout = "gb";
    };

    services.displayManager = {
      sddm = {enable = true;};
      defaultSession = "hyprland";
    };

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
