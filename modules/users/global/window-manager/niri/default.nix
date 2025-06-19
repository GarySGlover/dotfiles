{
  config,
  pkgs,
  lib,
  ...
}:
with lib;
let
  wlr-which-key =
    with pkgs;
    rustPlatform.buildRustPackage {
      pname = "wlr-which-key";
      version = "git"; # pretend

      src = fetchFromGitHub {
        owner = "MaxVerevkin";
        repo = "wlr-which-key";
        rev = "b42793066d6a1a6bd3c93053f3f0c69ff058ac7f";
        hash = "sha256-s7wu3mbMmcFAAGZAH5mZi+Ss1MMJ/urFObL7Jybf7V8=";
      };

      cargoHash = "sha256-QfJQ2n/nz/jb5dfbJ8UAnHeIsSd4zv+cMdPi4Vgob30=";

      nativeBuildInputs = [
        pkg-config
      ];

      buildInputs = [
        cairo
        glib
        libxkbcommon
        pango
      ];

      meta = with lib; {
        description = "Keymap manager for wlroots-based compositors";
        homepage = "https://github.com/MaxVerevkin/wlr-which-key";
        license = licenses.gpl3Only;
        maintainers = with maintainers; [ xlambein ];
        platforms = platforms.linux;
        mainProgram = "wlr-which-key";
      };
    };

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
  config = mkIf config.wolf.roles.desktop {
    home.packages =
      (with pkgs; [
        xwayland-satellite
        bluetuith
        hyprlock
      ])
      ++ [
        wlr-which-key
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
        "spawn-at-startup \"udiskie\"" = [ ];
        "spawn-at-startup \"kanshi\"" = [ ];
        "spawn-at-startup \"xwayland-satellite\"" = [ ];

        environment = {
          DISPLAY = ":0";
          XDG_CONFIG_HOME = "${config.xdg.configHome}";
        };

        binds = {
          "Ctrl+SemiColon" = {
            spawn = [
              "wlr-which-key"
              "${config.xdg.configHome}/niri/wlr-which-key-config.yaml"
            ];
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

    xdg.configFile."niri/wlr-which-key-config.yaml".source =
      (pkgs.formats.yaml { }).generate "wlr-which-key"
        {
          anchor = "center";
          inhibit_compositor_keyboard_shortcuts = true;
          menu = [
            {
              key = "a";
              desc = "Apps";
              submenu = [
                {
                  key = "b";
                  desc = "Brave";
                  cmd = "brave";
                }
                {
                  key = "B";
                  desc = "Bluetuith";
                  cmd = "kitty bluetuith";
                }
                {
                  key = "c";
                  desc = "Chromium";
                  cmd = "chromium";
                }
                {
                  key = "e";
                  desc = "Emacs";
                  cmd = "emacsclient --create-frame -no-wait --alternate-editor 'emacs'";
                }
                {
                  key = "f";
                  desc = "Firefox";
                  cmd = "firefox";
                }
                {
                  key = "l";
                  desc = "Lock";
                  cmd = "hyprlock";
                }
                {
                  key = "n";
                  desc = "Nyxt";
                  cmd = "nyxt";
                }
                {
                  key = "s";
                  desc = "Steam";
                  cmd = "steam";
                }
                {
                  key = "t";
                  desc = "Terminal";
                  cmd = "kitty";
                }
                {
                  key = "w";
                  desc = "Work Browser";
                  cmd = "floorp";
                }
              ];
            }
            {
              key = "f";
              cmd = "niri msg action focus-column-right-or-first";
              desc = "Column Right";
              keep_open = true;
            }
            {
              key = "F";
              cmd = "niri msg action focus-monitor-right";
              desc = "Monitor Right";
            }
            {
              key = "Ctrl+f";
              desc = "Move Column Right";
              cmd = "niri msg action move-column-right";
              keep_open = true;
            }
            {
              key = "Ctrl+F";
              desc = "Move Column to Monitor Right";
              cmd = "niri msg action move-column-to-monitor-right";
            }
            {
              key = "Alt+f";
              desc = "Move Workspace to Monitor Right";
              cmd = "niri msg action move-workspace-to-monitor-right";
            }
            {
              key = "b";
              cmd = "niri msg action focus-column-left-or-last";
              desc = "Column Left";
              keep_open = true;
            }
            {
              key = "B";
              cmd = "niri msg action focus-monitor-left";
              desc = "Monitor Left";
            }
            {
              key = "Ctrl+b";
              desc = "Move Column Left";
              cmd = "niri msg action move-column-left";
              keep_open = true;
            }
            {
              key = "Ctrl+B";
              desc = "Move Column to Monitor Left";
              cmd = "niri msg action move-column-to-monitor-left";
            }
            {
              key = "Alt+b";
              desc = "Move Workspace to Monitor Left";
              cmd = "niri msg action move-workspace-to-monitor-left";
            }
            {
              key = "p";
              cmd = "niri msg action focus-workspace-up";
              desc = "Workspace Up";
              keep_open = true;
            }
            {
              key = "P";
              cmd = "niri msg action focus-monitor-up";
              desc = "Monitor Up";
            }
            {
              key = "Ctrl+p";
              desc = "Move Column to Workspace Up";
              cmd = "niri msg action move-column-to-workspace-up";
              keep_open = true;
            }
            {
              key = "Ctrl+P";
              desc = "Move Column to Monitor Up";
              cmd = "niri msg action move-column-to-monitor-up";
            }
            {
              key = "Alt+p";
              desc = "Move Workspace to Monitor Up";
              cmd = "niri msg action move-workspace-to-monitor-up";
            }
            {
              key = "n";
              cmd = "niri msg action focus-workspace-down";
              desc = "Workspace Down";
              keep_open = true;
            }
            {
              key = "N";
              cmd = "niri msg action focus-monitor-down";
              desc = "Monitor Down";
            }
            {
              key = "Ctrl+n";
              desc = "Move Column to Workspace Down";
              cmd = "niri msg action move-column-to-workspace-down";
              keep_open = true;
            }
            {
              key = "Ctrl+N";
              desc = "Move Column to Monitor Down";
              cmd = "niri msg action move-column-to-monitor-down";
            }
            {
              key = "Alt+n";
              desc = "Move Workspace to Monitor Down";
              cmd = "niri msg action move-workspace-to-monitor-down";
            }
            {
              key = "v";
              desc = "Move Workspace Up";
              cmd = "niri msg action move-workspace-up";
              keep_open = true;
            }
            {
              key = "V";
              desc = "Move Workspace Down";
              cmd = "niri msg action move-workspace-down";
              keep_open = true;
            }
            {
              key = "e";
              desc = "Toggle Focus Floating";
              cmd = "niri msg action switch-focus-between-floating-and-tiling";
            }
            {
              key = "E";
              desc = "Toggle Window Floating";
              cmd = "niri msg action toggle-window-floating";
              keep_open = true;
            }
            {
              key = "s";
              desc = "Screenshot Window";
              cmd = "niri msg action screenshot-window";
            }
            {
              key = "S";
              desc = "Screenshot Screen";
              cmd = "niri msg action screenshot-screen";
            }
            {
              key = "Ctrl+s";
              desc = "Screenshot";
              cmd = "niri msg action screenshot";
            }
            {
              key = "c";
              desc = "Cast Window";
              cmd = "niri msg action set-dynamic-cast-window";
            }
            {
              key = "C";
              desc = "Cast Monitor";
              cmd = "niri msg action set-dynamic-cast-monitor";
            }
            {
              key = "Ctrl+C";
              desc = "Clear Cast";
              cmd = "niri msg action clear-dynamic-cast-target";
            }
            {
              key = "x";
              desc = "Switch Preset Column Width";
              cmd = "niri msg action switch-preset-column-width";
              keep_open = true;
            }
            {
              key = "X";
              desc = "Expand Column to Available Width";
              cmd = "niri msg action expand-column-to-available-width";
            }
            {
              key = "Ctrl+x";
              desc = "Maximize Column";
              cmd = "niri msg action maximize-column";
            }
            {
              key = "m";
              desc = "Maximize Column";
              cmd = "niri msg action maximize-column";
            }
            {
              key = "M";
              desc = "Fullscreen";
              cmd = "niri msg action fullscreen-window";
            }
            {
              key = "Ctrl+m";
              desc = "Fake Fullscreen";
              cmd = "niri msg action toggle-windowed-fullscreen";
            }
            {
              key = "c";
              desc = "Center column";
              cmd = "niri msg action center-column";
            }
            {
              key = "C";
              desc = "Center visible";
              cmd = "niri msg action center-visible-columns";
            }
            {
              key = "w";
              desc = "Close Window";
              cmd = "niri msg action close-window";

            }
            {
              key = "W";
              desc = "Quit";
              cmd = "niri msg action quit";
            }
          ];
        };
  };
}
