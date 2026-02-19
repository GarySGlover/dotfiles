# [[file:../../modules.org::*Niri binds][Niri binds:2]]
{
  flake.aspects.tiling-window-manager.homeManager = {
    niri.configFiles.binds.text = ''
      hotkey-overlay {
        hide-not-bound
      }
      binds {
        Mod+Alt+b hotkey-overlay-title="Column back" { move-column-left; }
        Mod+Alt+f hotkey-overlay-title="Column forward" { move-column-right; }
        Mod+Ctrl+b hotkey-overlay-title="Previous monitor" { focus-monitor-previous; }
        Mod+Ctrl+f hotkey-overlay-title="Next monitor" { focus-monitor-next; }
        Mod+b hotkey-overlay-title="Backwards column" { focus-column-left-or-last; }
        Mod+c hotkey-overlay-title="Cast window" { set-dynamic-cast-window; }
        Mod+f hotkey-overlay-title="Forward column" { focus-column-right-or-first; }
        Mod+h hotkey-overlay-title="Center column" { center-column; }
        Mod+m hotkey-overlay-title="Fullscreen" { fullscreen-window; }
        Mod+n hotkey-overlay-title="Next window" { focus-window-or-workspace-down; }
        Mod+p hotkey-overlay-title="Previous window" { focus-window-or-workspace-up; }
        Mod+s hotkey-overlay-title="Screenshot" { screenshot show-pointer=false; }
        Mod+x hotkey-overlay-title="Next width" { switch-preset-window-width; }
        Mod+Shift+c hotkey-overlay-title="Cast clear" { clear-dynamic-cast-target; }
        Mod+Shift+h hotkey-overlay-title="Center visible" { center-visible-columns; }
        Mod+Shift+m hotkey-overlay-title="Fullscreen alt" { toggle-windowed-fullscreen; }
        Mod+Shift+x hotkey-overlay-title="Previous width" { switch-preset-window-width-back; }
        Mod+Shift+slash hotkey-overlay-title=null { show-hotkey-overlay; }
      }
    '';
  };
}
# Niri binds:2 ends here
