{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    steam
    seatd
    mangohud
  ];
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
  };
  programs.gamescope.enable = true;
}
