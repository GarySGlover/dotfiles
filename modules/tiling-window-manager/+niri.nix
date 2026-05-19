# [[file:../../modules.org::*Niri][Niri:4]]
{
  flake.aspects.tilingWindowManager.homeManager =
    { pkgs, config, ... }:
    {
      home.packages = with pkgs; [
        wlr-which-key
      ];
      xdg.configFile."niri/wlr-which-key-config.yaml".text = ''
        anchor: center
        inhibit_compositor_keyboard_shortcuts: true
        menu:
        - cmd: swayosd-client --custom-message="$(wpctl inspect @DEFAULT_SINK@ | awk -F'=
            ' '/node.description/ {gsub(/"/, "", $2); print $2}')"
          desc: Audio Device
          key: a
        - cmd: kitty bluetuith
          desc: Bluetuith
          key: b
        - cmd: DRI_PRIME=1 chromium
          desc: Chromium
          key: c
        - cmd: swayosd-client --custom-message="$(date)"
          desc: Clock
          key: d
        - cmd: $EDITOR
          desc: Emacs
          key: e
        - cmd: firefox --no-remote -P home
          desc: Firefox
          key: f
        - cmd: gamescope -h 1080 --backend headless --adaptive-sync --hdr-enabled --rt --steam
            -- steam -pipewire-dmabuf -tenfoot
          desc: Steam
          key: h
        - cmd: kitty
          desc: Terminal
          key: k
        - cmd: hyprlock
          desc: Lock
          key: l
        - cmd: google-chrome-stable
          desc: Work Chrome
          key: r
        - cmd: gamescope -h 1080 --adaptive-sync --hdr-enabled --rt --steam -- steam -pipewire-dmabuf
            -tenfoot
          desc: Steam
          key: s
        - cmd: emacsclient --eval '(eshell t)' --create-frame -no-wait
          desc: Eshell
          key: t
        - cmd: firefox --no-remote -P work
          desc: Work Browser
          key: w
        - cmd: swayosd-client --custom-progress=$(awk '/[0-9]+/ {printf "%.2f\n", $0/100}'
            /sys/class/power_supply/BAT0/capacity) --custom-progress-text=Battery
          desc: Battery
          key: p
      '';
      niri.configFiles.binds.text = ''
        hotkey-overlay {
          hide-not-bound
        }
        binds {
          Mod+Minus { set-column-width "-8.333%"; }
          Mod+Shift+Equal { set-column-width "+8.333%"; }
          Mod+Equal { set-column-width "+8.333%"; }
          Mod+Shift+slash { show-hotkey-overlay; }
          Mod+slash { show-hotkey-overlay; }
          Mod+Alt+Minus { set-window-height "-8.333%"; }
          Mod+Alt+Shift+Equal { set-window-height "+8.333%"; }
          Mod+Alt+Equal { reset-window-height; }
          Mod+a  hotkey-overlay-title="Apps" { spawn "wlr-which-key" "${config.xdg.configHome}/niri/wlr-which-key-config.yaml"; }
          Mod+b  hotkey-overlay-title="Backward column" { focus-column-left-or-last; }
          Mod+c  hotkey-overlay-title="Cast window" { set-dynamic-cast-window; }
          Mod+d  hotkey-overlay-title="Move col left" { move-column-left; }
          Mod+e  hotkey-overlay-title="Move col right" { move-column-right; }
          Mod+f  hotkey-overlay-title="Forward column" { focus-column-right-or-first; }
          Mod+g  hotkey-overlay-title="Prev monitor" { focus-monitor-previous; }
          Mod+h  hotkey-overlay-title="Center column" { center-column; }
          Mod+j  hotkey-overlay-title="Col to next ws" { move-column-to-workspace-down; }
          Mod+k  hotkey-overlay-title="Col to prev ws" { move-column-to-workspace-up; }
          Mod+l  hotkey-overlay-title="Next monitor" { focus-monitor-next; }
          Mod+m  hotkey-overlay-title="Fullscreen" { toggle-windowed-fullscreen; }
          Mod+n  hotkey-overlay-title="Next window" { focus-window-or-workspace-down; }
          Mod+p  hotkey-overlay-title="Prev window" { focus-window-or-workspace-up; }
          Mod+q  hotkey-overlay-title="Cast clear" { clear-dynamic-cast-target; }
          Mod+s  hotkey-overlay-title="Screenshot" { screenshot show-pointer=false; }
          Mod+t  hotkey-overlay-title="Center visible" { center-visible-columns; }
          Mod+u  hotkey-overlay-title="Move ws up" { move-workspace-up; }
          Mod+v  hotkey-overlay-title="Move ws down" { move-workspace-down; }
          Mod+w  hotkey-overlay-title="WS to next mon" { move-workspace-to-monitor-next; }
          Mod+x  hotkey-overlay-title="Next width" { switch-preset-window-width; }
          Mod+z  hotkey-overlay-title="Prev width" { switch-preset-window-width-back; }
          Mod+Shift+m hotkey-overlay-title="Fullscreen alt" { fullscreen-window; }
        }
      '';
      niri.configFiles.base = {
        priority = 900;
        text = ''
          cursor {
            hide-after-inactive-ms 1000
            hide-when-typing
          }
          gestures {
            hot-corners {
              off
            }
          }
          input {
            disable-power-key-handling
            focus-follows-mouse max-scroll-amount="0%"
            warp-mouse-to-focus mode="center-xy-always"
            workspace-auto-back-and-forth
            keyboard {
              xkb {
                layout "gb"
              }
            }
            touchpad {
              drag true
              dwt
              tap-button-map "left-right-middle"
            }
          }
          prefer-no-csd
          recent-windows {
            off
          }
          screenshot-path "null"
        '';
      };
    };
}
# Niri:4 ends here
