# [[file:waybar.org::*Waybar][Waybar:2]]
{
  config,
  lib,
  ...
}:
let
  theme = config.wolf.theme;
  colors = theme.colors;
in
{
  config = lib.mkIf config.wolf.roles.desktop {
    programs.waybar = {
      enable = true;
      systemd.enable = false;
      settings = {
        mainbar = {
          layer = "top";
          position = "top";
          margin = "${toString theme.gaps}";
          exclusive = "true";
          modules-left = [
            "hyprland/workspaces"
            "hyprland/submap"
            "custom/kanata-layer"
          ];
          modules-center = [
            "hyprland/window"
          ];
          modules-right = [
            "pulseaudio"
            "battery"
            "clock"
          ];
          "hyprland/workspaces" = { };
          "hyprland/submap" = {
            always-on = "true";
          };
          "clock" = {
            format = "{:%Y%m%d-%H%M}";
            tooltip = "false";
          };
          pulseaudio = {
            format = "{desc} {volume}% {icon}";
            format-muted = "";
          };
          "custom/kanata-layer" = {
            exec = "${config.xdg.configHome}/waybar/kanata-layer";
            restart-interval = 1;
            format = "{}";
            return-type = "json";
          };
        };
      };
      style = ''
        * {
            font-family: "${theme.font.name}";
            font-size: ${toString theme.font.size}px;
            color: ${colors.strong};
        }
        window#waybar {
            background-color: ${colors.background};
            border: ${toString theme.border}px solid ${colors.weakBlue};
            border-radius: ${toString theme.radius}px;
        }
        window#waybar.fullscreen {
            color: ${colors.weakYellow};
        }
        #clock {
            padding: 0 0.5em;
            margin: 0.25em;
        }
        #battery {
            padding: 0 0.5em;
            margin: 0.25em;
        }
        #pulseaudio {
            padding: 0 0.5em;
            margin: 0.25em;
        }
        #custom-kanata-layer {
            padding-left: 0.5em;
            padding-right: 0.5em;
        }
      '';
    };
    xdg.configFile."waybar/kanata-layer".source = ./kanata-layer;
  };
}
# Waybar:2 ends here
