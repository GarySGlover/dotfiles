{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
{
  config = mkIf config.wolf.system.physical {
    programs.hyprland = {
      enable = true;
      xwayland.enable = true;
    };
    environment.systemPackages = with pkgs; [ hyprlandPlugins.hy3 ];

    programs.sway = {
      enable = true;
      wrapperFeatures.gtk = true;
    };

    services.xserver = {
      enable = true;
      # Configure keymap in X11
      xkb.layout = "gb";
      videoDrivers = [
        "modesetting"
      ];
    };

    services.displayManager = {
      sddm = {
        enable = true;
      };
      defaultSession = "sway";
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
