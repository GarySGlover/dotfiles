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
in
{
  config = mkIf config.wolf.roles.desktop {
    home.packages =
      (with pkgs; [
        xwayland-satellite
      ])
      ++ [
        wlr-which-key
      ];

    xdg.configFile."niri/config.kdl".text = lib.hm.generators.toKDL { } {
      input = {
        keyboard.xkb.layout = "gb";
        warp-mouse-to-focus = [ ];
        disable-power-key-handling = [ ];
        focus-follows-mouse = [ ];
        workspace-auto-back-and-forth = [ ];
      };
      cursor = {
        hide-after-inactaive-ms = 1000;
        hide-when-typing = [ ];
      };
      hotkey-overlay.skip-at-startup = [ ];
      layout = {
        # Columns
        always-center-single-column = [ ];
        center-focused-column = "on-overflow";
        default-column-width.proportion = 0.5;
        default-column-display = "tabbed";
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
      };

      binds = {
        "Ctrl+SemiColon" = {
          spawn = "wlr-which-key";
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
        "Shift+XF86AudioRaiseVolume" = {
          _props = {
            allow-when-locked = true;
          };
          spawn = [
            "brightnessctl"
            "set"
            "10%+"
          ];
        };
        "Shift+XF86AudioLowerVolume" = {
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

      window-rule = {
        geometry-corner-radius = 10;
        clip-to-geometry = true;
      };

    };

    xdg.configFile."wlr-which-key/config.yaml".source =
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
                  key = "c";
                  desc = "Chromium";
                  cmd = "chromium";
                }
                {
                  key = "e";
                  desc = "Emacs";
                  cmd = "emacsclient --create-frame -no-wait --alternate-editor ''";
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
              key = "w";
              desc = "Window Management";
              submenu = [
                {
                  key = "f";
                  cmd = "niri msg action focus-column-right";
                  desc = "Focus Column Right";
                  keep_open = true;
                }
                {
                  key = "F";
                  cmd = "niri msg action focus-column-right-or-first";
                  desc = "Focus Column Right or First";
                  keep_open = true;
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
                  keep_open = true;
                }
              ];
            }
            {
              key = "f";
              desc = "Focus Management";
              submenu = [
                {
                  key = "b";
                  desc = "Focus Column Left";
                  cmd = "niri msg action focus-column-left";
                  keep_open = true;
                }
                {
                  key = "p";
                  desc = "Focus Column Left or Last";
                  cmd = "niri msg action focus-column-left-or-last";
                  keep_open = true;
                }
                {
                  key = "t";
                  desc = "Focus Tiling";
                  cmd = "niri msg action focus-tiling";
                  keep_open = true;
                }
                {
                  key = "g";
                  desc = "Focus Floating";
                  cmd = "niri msg action focus-floating";
                  keep_open = true;
                }
                {
                  key = "l";
                  desc = "Focus Monitor Right";
                  cmd = "niri msg action focus-monitor-right";
                  keep_open = true;
                }
                {
                  key = "h";
                  desc = "Focus Monitor Left";
                  cmd = "niri msg action focus-monitor-left";
                  keep_open = true;
                }
                {
                  key = "k";
                  desc = "Focus Monitor Up";
                  cmd = "niri msg action focus-monitor-up";
                  keep_open = true;
                }
                {
                  key = "j";
                  desc = "Focus Monitor Down";
                  cmd = "niri msg action focus-monitor-down";
                  keep_open = true;
                }
                {
                  key = "m";
                  desc = "Focus Monitor Next";
                  cmd = "niri msg action focus-monitor-next";
                  keep_open = true;
                }
                {
                  key = "o";
                  desc = "Focus Monitor Previous";
                  cmd = "niri msg action focus-monitor-previous";
                  keep_open = true;
                }
                {
                  key = "u";
                  desc = "Focus Window Up";
                  cmd = "niri msg action focus-window-up";
                  keep_open = true;
                }
                {
                  key = "d";
                  desc = "Focus Window Down";
                  cmd = "niri msg action focus-window-down";
                  keep_open = true;
                }
                {
                  key = "y";
                  desc = "Focus Window Up or Bottom";
                  cmd = "niri msg action focus-window-up-or-bottom";
                  keep_open = true;
                }
                {
                  key = "v";
                  desc = "Focus Window Down or Top";
                  cmd = "niri msg action focus-window-down-or-top";
                  keep_open = true;
                }
                {
                  key = "r";
                  desc = "Focus Window Previous";
                  cmd = "niri msg action focus-window-previous";
                  keep_open = true;
                }
                {
                  key = "s";
                  desc = "Focus Window Bottom";
                  cmd = "niri msg action focus-window-bottom";
                  keep_open = true;
                }
                {
                  key = "a";
                  desc = "Focus Window Top";
                  cmd = "niri msg action focus-window-top";
                  keep_open = true;
                }
                {
                  key = "x";
                  desc = "Focus Workspace Up";
                  cmd = "niri msg action focus-workspace-up";
                  keep_open = true;
                }
                {
                  key = "z";
                  desc = "Focus Workspace Down";
                  cmd = "niri msg action focus-workspace-down";
                  keep_open = true;
                }
                {
                  key = "c";
                  desc = "Focus Workspace Previous";
                  cmd = "niri msg action focus-workspace-previous";
                  keep_open = true;
                }
              ];
            }
            {
              key = "m";
              desc = "Move Management";
              submenu = [
                {
                  key = "b";
                  desc = "Move Column Left";
                  cmd = "niri msg action move-column-left";
                  keep_open = true;
                }
                {
                  key = "p";
                  desc = "Move Column to First";
                  cmd = "niri msg action move-column-to-first";
                  keep_open = true;
                }
                {
                  key = "n";
                  desc = "Move Column to Last";
                  cmd = "niri msg action move-column-to-last";
                  keep_open = true;
                }
                {
                  key = "h";
                  desc = "Move Column to Monitor Left";
                  cmd = "niri msg action move-column-to-monitor-left";
                  keep_open = true;
                }
                {
                  key = "k";
                  desc = "Move Column to Monitor Up";
                  cmd = "niri msg action move-column-to-monitor-up";
                  keep_open = true;
                }
                {
                  key = "j";
                  desc = "Move Column to Monitor Down";
                  cmd = "niri msg action move-column-to-monitor-down";
                  keep_open = true;
                }
                {
                  key = "m";
                  desc = "Move Column to Monitor Next";
                  cmd = "niri msg action move-column-to-monitor-next";
                  keep_open = true;
                }
                {
                  key = "o";
                  desc = "Move Column to Monitor Previous";
                  cmd = "niri msg action move-column-to-monitor-previous";
                  keep_open = true;
                }
                {
                  key = "x";
                  desc = "Move Column to Workspace Up";
                  cmd = "niri msg action move-column-to-workspace-up";
                  keep_open = true;
                }
                {
                  key = "z";
                  desc = "Move Column to Workspace Down";
                  cmd = "niri msg action move-column-to-workspace-down";
                  keep_open = true;
                }
                {
                  key = "u";
                  desc = "Move Window Up";
                  cmd = "niri msg action move-window-up";
                  keep_open = true;
                }
                {
                  key = "d";
                  desc = "Move Window Down";
                  cmd = "niri msg action move-window-down";
                  keep_open = true;
                }
                {
                  key = "t";
                  desc = "Move Window to Floating";
                  cmd = "niri msg action move-window-to-floating";
                  keep_open = true;
                }
                {
                  key = "g";
                  desc = "Move Window to Tiling";
                  cmd = "niri msg action move-window-to-tiling";
                  keep_open = true;
                }
                {
                  key = "l";
                  desc = "Move Window to Monitor Right";
                  cmd = "niri msg action move-window-to-monitor-right";
                  keep_open = true;
                }
                {
                  key = "h";
                  desc = "Move Window to Monitor Left";
                  cmd = "niri msg action move-window-to-monitor-left";
                  keep_open = true;
                }
                {
                  key = "k";
                  desc = "Move Window to Monitor Up";
                  cmd = "niri msg action move-window-to-monitor-up";
                  keep_open = true;
                }
                {
                  key = "j";
                  desc = "Move Window to Monitor Down";
                  cmd = "niri msg action move-window-to-monitor-down";
                  keep_open = true;
                }
                {
                  key = "m";
                  desc = "Move Window to Monitor Next";
                  cmd = "niri msg action move-window-to-monitor-next";
                  keep_open = true;
                }
                {
                  key = "o";
                  desc = "Move Window to Monitor Previous";
                  cmd = "niri msg action move-window-to-monitor-previous";
                  keep_open = true;
                }
                {
                  key = "x";
                  desc = "Move Window to Workspace Up";
                  cmd = "niri msg action move-window-to-workspace-up";
                  keep_open = true;
                }
                {
                  key = "z";
                  desc = "Move Window to Workspace Down";
                  cmd = "niri msg action move-window-to-workspace-down";
                  keep_open = true;
                }
                {
                  key = "r";
                  desc = "Move Workspace Up";
                  cmd = "niri msg action move-workspace-up";
                  keep_open = true;
                }
                {
                  key = "s";
                  desc = "Move Workspace Down";
                  cmd = "niri msg action move-workspace-down";
                  keep_open = true;
                }
                {
                  key = "p";
                  desc = "Move Workspace to Monitor Up";
                  cmd = "niri msg action move-workspace-to-monitor-up";
                  keep_open = true;
                }
                {
                  key = "n";
                  desc = "Move Workspace to Monitor Down";
                  cmd = "niri msg action move-workspace-to-monitor-down";
                  keep_open = true;
                }
                {
                  key = "l";
                  desc = "Move Workspace to Monitor Right";
                  cmd = "niri msg action move-workspace-to-monitor-right";
                  keep_open = true;
                }
                {
                  key = "h";
                  desc = "Move Workspace to Monitor Left";
                  cmd = "niri msg action move-workspace-to-monitor-left";
                  keep_open = true;
                }
                {
                  key = "m";
                  desc = "Move Workspace to Monitor Next";
                  cmd = "niri msg action move-workspace-to-monitor-next";
                  keep_open = true;
                }
                {
                  key = "o";
                  desc = "Move Workspace to Monitor Previous";
                  cmd = "niri msg action move-workspace-to-monitor-previous";
                  keep_open = true;
                }
                {
                  key = "e";
                  desc = "Consume Window into Column";
                  cmd = "niri msg action consume-window-into-column";
                  keep_open = true;
                }
                {
                  key = "q";
                  desc = "Expel Window from Column";
                  cmd = "niri msg action expel-window-from-column";
                  keep_open = true;
                }
              ];
            }
            {
              key = "g";
              desc = "General Management";
              submenu = [
                {
                  key = "c";
                  desc = "Center Window";
                  cmd = "niri msg action center-window";

                }
                {
                  key = "v";
                  desc = "Center Column";
                  cmd = "niri msg action center-column";
                }
                {
                  key = "x";
                  desc = "Close Window";
                  cmd = "niri msg action close-window";

                }
                {
                  key = "f";
                  desc = "Fullscreen Window";
                  cmd = "niri msg action fullscreen-window";

                }
                {
                  key = "t";
                  desc = "Toggle Window Floating";
                  cmd = "niri msg action toggle-window-floating";

                }
                {
                  key = "e";
                  desc = "Expand Column to Available Width";
                  cmd = "niri msg action expand-column-to-available-width";
                }
                {
                  key = "m";
                  desc = "Maximize Column";
                  cmd = "niri msg action maximize-column";
                }
                {
                  key = "r";
                  desc = "Toggle Column Tabbed Display";
                  cmd = "niri msg action toggle-column-tabbed-display";
                }
                {
                  key = "s";
                  desc = "Switch Focus Between Floating and Tiling";
                  cmd = "niri msg action switch-focus-between-floating-and-tiling";
                }
                {
                  key = "p";
                  desc = "Switch Preset Column Width";
                  cmd = "niri msg action switch-preset-column-width";
                  keep_open = true;
                }
                {
                  key = "n";
                  desc = "Switch Preset Window Height";
                  cmd = "niri msg action switch-preset-window-height";
                  keep_open = true;
                }
                {
                  key = "l";
                  desc = "Switch Preset Window Width";
                  cmd = "niri msg action switch-preset-window-width";
                  keep_open = true;
                }
                {
                  key = "q";
                  desc = "Quit";
                  cmd = "niri msg action quit";
                }
                {
                  key = "a";
                  desc = "Screenshot";
                  cmd = "niri msg action screenshot";
                }
                {
                  key = "b";
                  desc = "Screenshot Screen";
                  cmd = "niri msg action screenshot-screen";
                }
                {
                  key = "d";
                  desc = "Screenshot Window";
                  cmd = "niri msg action screenshot-window";
                }
              ];
            }
          ];
        };
  };
}
