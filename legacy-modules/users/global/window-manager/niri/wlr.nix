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
                  key = "e";
                  desc = "Emacs";
                  cmd = "$EDITOR";
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
                  key = "S";
                  desc = "Steam";
                  cmd = "gamescope -h 1080 --backend headless --adaptive-sync --hdr-enabled --rt --steam -- steam -pipewire-dmabuf -tenfoot";
                }
                {
                  key = "t";
                  desc = "Eshell";
                  cmd = "emacsclient --eval '(eshell t)' --create-frame -no-wait";
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
              key = "g";
              desc = "Close WLR";
              cmd = "";
            }
          ];
        };
  };
}
