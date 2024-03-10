{pkgs, ...}: {
  home.packages = with pkgs; [
    btop
    lm_sensors
  ];
}
