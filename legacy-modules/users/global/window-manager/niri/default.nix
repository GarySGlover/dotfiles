{
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
  config = {
    home.packages = with pkgs; [
      (wl-kbptr.overrideAttrs (_: {
        src = fetchFromGitHub {
          owner = "moverest";
          repo = "wl-kbptr";
          rev = "1c6c9275a49f6def4c37707e741da47f5098be7c";
          sha256 = "sha256-UEVPeqD1Oj3cK2Hq2eLpGy6Jdjd9i0tQNXdiDWAUIM0=";
        };
        version = "unstable-local";
      }))
    ];

    niri.configFiles.legacy = {
      priority = 1;
      text =
        lib.hm.generators.toKDL { } {
          layout = {
            # Columns
            always-center-single-column = [ ];
            center-focused-column = "never";
            default-column-width.proportion = 0.33;
            default-column-display = "tabbed";
            tab-indicator = {
              hide-when-single-tab = [ ];
            };
            preset-column-widths = {
              "proportion 0.33" = [ ];
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

          "spawn-at-startup \"emacs --bg-daemon\"" = [ ];

          binds = {
            "Mod+Period" = {
              spawn = [
                "wl-kbptr"
                "-o"
                "modes=tile,bisect"
                "-o"
                "home_row_keys=isrtneaoghb"
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
  };
}
