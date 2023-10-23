{
  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
  };

  services.xserver = {
    enable = true;
    displayManager.sddm = {enable = true;};
    #displayManager.startx.enable = true;
  };

  # Configure keymap in X11
  services.xserver.layout = "gb";

  # Enable sound.
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };
}
