{
  config,
  pkgs,
  lib,
  ...
}:
with lib;
let
  windowRules = [
    {
      "match is-window-cast-target=true" = [ ];
      border = {
        inactive-color = "#f38ba8";
        active-color = "#f38ba8";
      };
    }
    {
      border = {
        active-color = "#64B5F6";
      };
      focus-ring = {
        active-color = "#64B5F6";
      };
      clip-to-geometry = true;
    }
    {
      "match title=\"Org Capture\" app-id=\"emacs\"" = [ ];
      "match title=\"Emacs Everywhere\" app-id=\"emacs\"" = [ ];
      default-column-width = {
        proportion = 0.5;
      };
      default-window-height = {
        proportion = 0.5;
      };
      open-floating = true;
      open-focused = true;
      open-fullscreen = false;
      "default-floating-position x=30 y=30 relative-to=\"top-right\"" = [ ];
    }
    {
      "match app-id=\"gamescope\"" = [ ];
      open-focused = true;
      open-fullscreen = true;
    }
  ];
  renderedWindowRules = builtins.concatStringsSep "\n\n" (
    map (rule: lib.hm.generators.toKDL { } { "window-rule" = rule; }) windowRules
  );
in
{
  imports = [
    ./wlr.nix
  ];
  config = mkIf config.wolf.roles.wayland {
    systemd.user.services.swayosd-server = {
      Unit = {
        Description = "Sway OSD Server";
        After = [ "graphical-session.target" ];
      };
      Service = {
        ExecStart = "${pkgs.swayosd}/bin/swayosd-server";
        Restart = "always";
        RestartSec = 5;
      };
      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };

    systemd.user.services.emacs-wm-daemon = {
      Unit = {
        Description = "Emacs daemon for WM (emacs --daemon=wm)";
      };
      Service = {
        ExecStart = "${pkgs.runtimeShell} -c 'exec emacs --fg-daemon=wm'";
        SuccessExitStatus = 15;
        Restart = "always";
      };
      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };

    home.packages = with pkgs; [
      wbg
      bluetuith
      hyprlock
      swayosd
      (wl-kbptr.overrideAttrs (old: {
        src = fetchFromGitHub {
          owner = "moverest";
          repo = "wl-kbptr";
          rev = "1c6c9275a49f6def4c37707e741da47f5098be7c";
          sha256 = "sha256-UEVPeqD1Oj3cK2Hq2eLpGy6Jdjd9i0tQNXdiDWAUIM0=";
        };
        version = "unstable-local";
      }))
      xwayland-satellite
    ];

    xdg.configFile."niri/config.kdl".text =
      lib.hm.generators.toKDL { } {
        prefer-no-csd = [ ];
        screenshot-path = "null";
        input = {
          keyboard.xkb = {
            layout = "gb";
          };
          touchpad = {
            tap = [ ];
            dwt = [ ];
            drag = true;
            tap-button-map = "left-right-middle";
          };
          warp-mouse-to-focus = [ ];
          disable-power-key-handling = [ ];
          workspace-auto-back-and-forth = [ ];
          "focus-follows-mouse max-scroll-amount=\"0%\"" = [ ];
        };
        cursor = {
          hide-after-inactive-ms = 1000;
          hide-when-typing = [ ];
        };
        hotkey-overlay.skip-at-startup = [ ];
        gestures.hot-corners.off = [ ];
        layout = {
          # Columns
          always-center-single-column = [ ];
          center-focused-column = "on-overflow";
          default-column-width.proportion = 1.0;
          default-column-display = "tabbed";
          tab-indicator = {
            hide-when-single-tab = [ ];
          };
          preset-column-widths = {
            "proportion 0.33333" = [ ];
            "proportion 0.5" = [ ];
            "proportion 1.0" = [ ];
          };

          # Window
          gaps = 4;
          border.width = 2;

          # Workspace
          empty-workspace-above-first = [ ];

          # Rows
          preset-window-heights = {
            "proportion 0.33333" = [ ];
            "proportion 0.5" = [ ];
            "proportion 0.66667" = [ ];
            "proportion 1.0" = [ ];
          };

          focus-ring = {
            active-color = "#7fc8ff";
            inactive-color = "#7fc8ff";
          };
        };

        environment = {
          DISPLAY = ":0";
          XDG_CONFIG_HOME = "${config.xdg.configHome}";
        };

        recent-windows = {
          # Remove default binds as these conflict with some of the
          # emacs config for binds
          binds = {
          };
        };

        binds = {
          "Super+C" = {
            spawn = [
              "emacsclient"
              "--socket-name=wm"
              "-e"
              "(cnit-wm-org-capture)"
            ];
          };

          "Super+E" = {
            spawn = [
              "emacsclient"
              "--socket-name=wm"
              "-e"
              "(emacs-everywhere)"
            ];
          };

          "Super+T" = {
            spawn = [ "kitty" ];
          };
          "Ctrl+SemiColon" = {
            spawn = [
              "wlr-which-key"
              "${config.xdg.configHome}/niri/wlr-which-key-config.yaml"
            ];
          };
          "Ctrl+Shift+Period" = {
            spawn = [
              "wl-kbptr"
              "-o"
              "modes=tile,bisect"
              "-o"
              "home_row_keys=isrtneaoghb"
            ];
          };
          XF86AudioRaiseVolume = {
            _props = {
              allow-when-locked = true;
            };
            spawn = [
              "swayosd-client"
              "--output-volume=raise"
            ];
          };
          XF86AudioLowerVolume = {
            _props = {
              allow-when-locked = true;
            };
            spawn = [
              "swayosd-client"
              "--output-volume=lower"
            ];
          };
          XF86AudioMute = {
            _props = {
              allow-when-locked = true;
            };
            spawn = [
              "swayosd-client"
              "--output-volume=mute-toggle"
            ];
          };
          XF86AudioMicMute = {
            _props = {
              allow-when-locked = true;
            };
            spawn = [
              "swayosd-client"
              "--input-volume=mute-toggle"
            ];
          };
          "XF86MonBrightnessUp" = {
            _props = {
              allow-when-locked = true;
            };
            spawn = [
              "swayosd-client"
              "--brigtness=raise"
            ];
          };
          "XF86MonBrightnessDown" = {
            _props = {
              allow-when-locked = true;
            };
            spawn = [
              "swayosd-client"
              "--brigtness=lower"
            ];
          };

        };
      }
      + "\n"
      + renderedWindowRules;
  };
}
