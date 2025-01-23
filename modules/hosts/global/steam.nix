{ ... }:
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
}
