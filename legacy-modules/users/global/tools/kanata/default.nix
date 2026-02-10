{
  config,
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [ kanata ];
  home.file."${config.xdg.configHome}/kanata/kanata.kbd".source = ./kanata.kbd;

  systemd.user.services.kanata = {
    Unit = {
      Description = "Kanata keyboard remapper";
    };

    Service = {
      Type = "simple";
      ExecStart = "${pkgs.kanata}/bin/kanata --cfg ${config.xdg.configHome}/kanata/kanata.kbd -q -n -p 42001";
      Restart = "always";
    };

    Install = {
      WantedBy = [ "default.target" ];
    };
  };
}
