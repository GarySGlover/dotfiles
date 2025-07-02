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
      geometry-corner-radius = 10;
      clip-to-geometry = true;
    }
  ];
  renderedWindowRules = builtins.concatStringsSep "\n\n" (
    map (rule: lib.hm.generators.toKDL { } { "window-rule" = rule; }) windowRules
  );
in
{
  imports = [
    ./wlr.nix
    ./ags.nix
  ];
  config = mkIf config.wolf.roles.wayland {
    home.packages = with pkgs; [
      xwayland-satellite
      bluetuith
      hyprlock
    ];

    xdg.configFile."niri/config.kdl".text =
      lib.hm.generators.toKDL { } {
        prefer-no-csd = [ ];
        input = {
          keyboard.xkb.layout = "gb";
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
          default-column-width.proportion = 0.5;
          default-column-display = "tabbed";
          tab-indicator = {
            hide-when-single-tab = [ ];
          };
          preset-column-widths = {
            "proportion 0.25" = [ ];
            "proportion 0.33333" = [ ];
            "proportion 0.5" = [ ];
            "proportion 0.66667" = [ ];
            "proportion 0.75" = [ ];
            "proportion 1.0" = [ ];
          };

          # Window
          gaps = 4;
          border.width = 2;

          # Workspace
          empty-workspace-above-first = [ ];
          struts = {
            left = 16;
            right = 16;
          };

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

        "spawn-at-startup \"waybar\"" = [ ];
        "spawn-at-startup \"ags\" \"run\" \"--gtk4\"" = [ ];
        "spawn-at-startup \"udiskie\"" = [ ];
        "spawn-at-startup \"kanshi\"" = [ ];
        "spawn-at-startup \"xwayland-satellite\"" = [ ];

        environment = {
          DISPLAY = ":0";
          XDG_CONFIG_HOME = "${config.xdg.configHome}";
        };

        binds = {
          "Alt+SemiColon" = {
            spawn = [
              "wlr-which-key"
              "${config.xdg.configHome}/niri/wlr-which-key-config.yaml"
            ];
          };
          "Ctrl+SemiColon" = {
            spawn = [
              "ags"
              "request"
              "toggleMenu"
            ];
          };
          "Super+T" = {
            spawn = [ "kitty" ];
          };

          XF86AudioRaiseVolume = {
            _props = {
              allow-when-locked = true;
            };
            spawn = [
              "wpctl"
              "set-volume"
              "@DEFAULT_AUDIO_SINK@"
              "0.1+"
            ];
          };
          XF86AudioLowerVolume = {
            _props = {
              allow-when-locked = true;
            };
            spawn = [
              "wpctl"
              "set-volume"
              "@DEFAULT_AUDIO_SINK@"
              "0.1-"
            ];
          };
          "XF86MonBrightnessUp" = {
            _props = {
              allow-when-locked = true;
            };
            spawn = [
              "brightnessctl"
              "set"
              "10%+"
            ];
          };
          "XF86MonBrightnessDown" = {
            _props = {
              allow-when-locked = true;
            };
            spawn = [
              "brightnessctl"
              "set"
              "10%-"
            ];
          };
          XF86AudioMute = {
            _props = {
              allow-when-locked = true;
            };
            spawn = [
              "wpctl"
              "set-mute"
              "@DEFAULT_AUDIO_SINK@"
              "toggle"
            ];
          };
          XF86AudioMicMute = {
            _props = {
              allow-when-locked = true;
            };
            spawn = [
              "wpctl"
              "set-mute"
              "@DEFAULT_AUDIO_SOURCE@"
              "toggle"
            ];
          };
        };
      }
      + "\n"
      + renderedWindowRules;
  };
}
