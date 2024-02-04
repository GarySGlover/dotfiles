{
  config,
  lib,
  ...
}:
with lib; {
  config = {
    programs.starship = {
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = true;
      enableFishIntegration = true;
      settings = {
        battery = {
          full_symbol = "󱊣 ";
          charging_symbol = "󱊦 ";
          discharging_symbol = "󰂂 ";
          display = [
            {
              threshold = 90;
              style = "bold green";
            }
            {
              threshold = 30;
              style = "bold yellow";
            }
            {
              threshold = 99;
              style = "bold red";
            }
          ];
        };
        cmd_duration = {
          min_time = 0;
          show_milliseconds = true;
        };
        hostname = {
          ssh_only = false;
        };
        time = {
          disabled = false;
        };
        # custom.direnv = {
        #   format = "[\\[direnv\\]]($style) ";
        #   style = "fg:yellow dimmed";
        #   when = "env | grep -E '^DIRENV_FILE='";
        # };
      };
    };
  };
}
