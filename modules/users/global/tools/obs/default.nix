{ pkgs, ... }:
{
  programs.obs-studio = {
    enable = true;
    plugins = with pkgs.obs-studio-plugins; [
      obs-backgroundremoval
      obs-websocket
    ];
  };

  home.packages = with pkgs; [
    obs-cli
    ffmpeg
    v4l-utils
  ];

  systemd.user.services.obs-virtualcam = {
    Unit = {
      Description = "OBS headless virtual camera";
      After = [ "graphical-session.target" ];
    };

    Service = {
      ExecStart = "${pkgs.obs-studio}/bin/obs --startvirtualcam --collection virtualcam --scene idle --minimize-to-tray";
      Restart = "on-failure";
      Environment = [
        "DISPLAY=:0"
        "XDG_RUNTIME_DIR=/run/user/%U"
      ];
    };

    Install = {
      # WantedBy = [ "default.target" ];
    };
  };

  # xdg.configFile."obs-studio/basic/scenes/virtualcam.json".source = ./scenes/virtualcam.json; # pre-exported scene collection
}
