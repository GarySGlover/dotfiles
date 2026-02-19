# [[file:../../modules.org::*Niri base config][Niri base config:2]]
{
  flake.aspects.tiling-window-manager.homeManager = {
    niri.configFiles.base = {
      # priority = 990;
      text = ''
        cursor {
          hide-after-inactive-ms 1000
          hide-when-typing
        }
        environment {
          DISPLAY ":0"
          XDG_CONFIG_HOME "/home/clover/.config"
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
# Niri base config:2 ends here
