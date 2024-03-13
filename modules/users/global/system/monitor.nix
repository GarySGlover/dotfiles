{pkgs, ...}: {
  home.packages = with pkgs; [
    lm_sensors
  ];

  programs.btop = {
    enable = true;
    settings = {
      force_tty = true;
    };
  };
}
