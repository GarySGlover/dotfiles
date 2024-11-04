# [[file:waybar.org::*Waybar][Waybar:2]]
{
  config,
  lib,
  ...
}:
let
  theme = config.wolf.theme;
  faces = theme.faces;
in
{
  config = lib.mkIf config.wolf.roles.desktop {
    programs.waybar = {
      enable = true;
      systemd.enable = true;
      settings = {
        mainbar = {
          layer = "top";
          position = "top";
          margin = "${toString theme.font.size}";
          exclusive = "true";
          modules-left = [
            "hyprland/workspaces"
            "hyprland/submap"
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
        };
      };
      style = ''
        * {
            font-family: ${theme.font.name};
            font-size: ${toString theme.font.size}px;
            color: #${faces.fgFontLockVariableNameFace};
        }

        window#waybar {
            background-color: #${faces.bgDefault};
            border: ${toString theme.border}px solid #${faces.fgBorderInactive};
            border-radius: ${toString theme.radius}px;
        }
        window#waybar.fullscreen {
            color: #${faces.fgFontLockWarningFace};
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

        #workspaces {
            border-radius: ${toString theme.radius}px;
            background-color: #${faces.bgDefault};
            padding: 0 0.5em;
        }
        #workspaces button {
            border-radius: ${toString theme.radius}px;
            background-color: #${faces.bgTabBarTabInactive};
        }
        #workspaces button.active {
            background-color: #${faces.bgTabBarTab};
        }

        #submap {
            color: #${faces.fgFontLockVariableNameFace};
        }
        #submap.command {
            color: #${faces.fgFontLockStringFace};
        }
        #submap.insert {
            color: #${faces.fgFontLockWarningFace};
        }
      '';
    };
  };
}
# Waybar:2 ends here
