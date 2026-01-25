{ pkgs, ... }:
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
  config = {
    home.packages = [
      wlr-which-key
    ];
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
                  cmd = "DRI_PRIME=1 chromium";
                }
                {
                  key = "E";
                  desc = "Emacs (new)";
                  cmd = "emacs --init-directory=~/dotfiles/modules/users/global/editors/emacs/emacs-new/";
                }
                {
                  key = "e";
                  desc = "Emacs";
                  cmd = "emacsclient --create-frame -no-wait --alternate-editor 'emacs'";
                }
                {
                  key = "f";
                  desc = "Firefox";
                  cmd = "firefox --no-remote -P home";
                }
                {
                  key = "l";
                  desc = "Lock";
                  cmd = "hyprlock";
                }
                {
                  key = "s";
                  desc = "Steam";
                  cmd = "gamescope -h 1080 --adaptive-sync --hdr-enabled --rt --steam -- steam -pipewire-dmabuf -tenfoot";
                }
                {
                  key = "t";
                  desc = "Eshell";
                  cmd = "emacsclient --eval '(eshell t)' --create-frame -no-wait --alternate-editor 'emacs --eval'";
                }

                {
                  key = "T";
                  desc = "Terminal";
                  cmd = "kitty";
                }
                {
                  key = "w";
                  desc = "Work Browser";
                  cmd = "firefox --no-remote -P work";
                }
                {
                  key = "W";
                  desc = "Work Chrome";
                  cmd = "google-chrome-stable";
                }
                {
                  key = "x";
                  desc = "AGS Which Key";
                  cmd = "ags request toggleMenus";
                }
              ];
            }
            {
              key = "o";
              desc = "Overlay";
              submenu = [
                {
                  key = "a";
                  desc = "Audio Device";
                  cmd = "swayosd-client --custom-message=\"$(wpctl inspect @DEFAULT_SINK@ | awk -F'= ' '/node.description/ {gsub(/\"/, \"\", $2); print $2}')\"";
                }
                {
                  key = "b";
                  desc = "Battery";
                  cmd = "swayosd-client --custom-progress=$(awk '/[0-9]+/ {printf \"%.2f\\n\", $0/100}' /sys/class/power_supply/BAT0/capacity) --custom-progress-text=Battery";
                }
                {
                  key = "c";
                  desc = "Clock";
                  cmd = "swayosd-client --custom-message=\"$(date)\"";
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
              key = "Ctrl+Alt+f";
              desc = "Consume or expel the focused window right";
              cmd = "niri msg action consume-or-expel-window-right";
              keep_open = true;
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
              key = "Ctrl+Alt+b";
              desc = "Consume or expel the focused window left";
              cmd = "niri msg action consume-or-expel-window-left";
              keep_open = true;
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
              key = "u";
              desc = "Focus the window above";
              cmd = "niri msg action focus-window-up";
              keep_open = true;
            }
            {
              key = "Ctrl+u";
              desc = "Move the window up";
              cmd = "niri msg action move-window-up";
              keep_open = true;
            }
            {
              key = "d";
              desc = "Focus the window below";
              cmd = "niri msg action focus-window-down";
              keep_open = true;
            }
            {
              key = "Ctrl+d";
              desc = "Move the window down";
              cmd = "niri msg action move-window-down";
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
              desc = "Fake Fullscreen";
              cmd = "niri msg action toggle-windowed-fullscreen";
            }
            {
              key = "M";
              desc = "Fullscreen";
              cmd = "niri msg action fullscreen-window";
            }
            {
              key = "h";
              desc = "Center column";
              cmd = "niri msg action center-column";
            }
            {
              key = "H";
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
            {
              key = "g";
              desc = "Close WLR";
              cmd = "";
            }
          ];
        };
  };
}
