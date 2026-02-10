{ pkgs, ... }:
{
  programs = {
    gamescope = {
      enable = true;
      capSysNice = true;
      args = [
        "--rt" # realtime
        "--adaptive-sync"
      ];
    };
    steam = {
      enable = true;
      gamescopeSession.enable = true;
    };
  };
  hardware.xone.enable = true;
  hardware.steam-hardware.enable = true;
  environment.systemPackages = [ pkgs.mangohud ];

  programs.fish.shellAliases = {
    gs = ''
      env MANGOHUD=1 MANGOHUD_CONFIG=cpu_temp,gpu_temp,ram,vram \
        gamescope \
          --adaptive-sync \
          --hdr-enabled \
          --mangoapp \
          --rt \
          --steam \
          -- steam -pipewire-dmabuf -tenfoot
    '';
  };
}
