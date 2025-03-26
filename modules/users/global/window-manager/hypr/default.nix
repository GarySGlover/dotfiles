{
  config,
  pkgs,
  lib,
  ...
}:
with lib;
let
  theme = config.wolf.theme;
  faces = theme.faces;
  keybinder = (
    import ./devil.nix {
      inherit pkgs lib;
    }
  );
in
{
  config = mkIf config.wolf.roles.desktop {
    home.file."${config.xdg.configHome}/hypr/hypr-insert.sh".source = ./hypr-insert.sh;
    home.packages = with pkgs; [
      libnotify
    ];

    programs.hyprlock = {
      enable = true;
      settings = {
        general = {
          disable_loading_bar = true;
          hide_cursor = true;
        };
        background = [
          {
            color = "rgb(${faces.bgDefault})";
          }
        ];
        input-field = [
          {
            outer_color = "rgb(${faces.fgBorder})";
            inner_color = "rgb(${faces.bgDefault})";
            font_color = "rgb(${faces.fgDefault})";
            font_family = theme.font.name;
            fade_on_empty = false;
            placeholder_text = "<i>Password...</i>";
          }
        ];
      };
    };

    wayland.windowManager.hyprland = {
      enable = true;
      settings = {
        workspace = builtins.map (w: "${w.id}, defaultName:${w.name}") keybinder.workSpaces;
        windowrulev2 = [
          "workspace special:room101 silent, title:^(meet.google.com is sharing a window.)$"
          "float, initialTitle:(MainPicker)"
          "bordercolor rgb(FF0000) rgb(880808), fullscreen:1"
        ] ++ (if config.wolf.roles.gaming then [ "workspace 10,class:(steam)" ] else [ ]);

        binds = {
          window_direction_monitor_fallback = false;
        };

        cursor = {
          inactive_timeout = 2;
          persistent_warps = true;
          hide_on_key_press = true;
        };

        exec-once = [
          "${pkgs.hyprland}/bin/hyprctl dispatch submap insert" # Start in the modal command mappings
          "udiskie &" # Disk auto mount
          "waybar &"
          "kanshi &"
        ];
        exec = [
          "kanshictl reload" # Force monitor refresh on reload
          "${pkgs.glib}/bin/gsettings set org.gnome.desktop.interface gtk-theme \"Dracula\""
          "${pkgs.glib}/bin/gsettings set org.gnome.desktop.interface color-scheme \"prefer-dark\""
        ];
        env = [
          "QT_QPA_PLATFORMTHEME,qt6ct"
        ];

        general = {
          layout = "scroller";
          border_size = theme.border;
          gaps_in = theme.gaps;
          gaps_out = theme.gaps;
          "col.inactive_border" = "rgb(${faces.fgBorderInactive})";
          "col.active_border" = "rgb(${faces.fgBorder})";
          "col.nogroup_border" = "rgb(${faces.fgBorderInactive})";
          "col.nogroup_border_active" = "rgb(${faces.fgBorder})";
        };
        master = {
          mfact = 0.5;
          orientation = "left";
        };
        input = {
          kb_layout = "gb";
          kb_options = "ctrl:nocaps";
          touchpad = {
            disable_while_typing = false;
            middle_button_emulation = true;
            natural_scroll = true;
            tap-to-click = false;
          };
        };

        decoration = {
          shadow = {
            enabled = false;
          };
          rounding = toString theme.radius;
        };

        misc = {
          disable_hyprland_logo = true;
          font_family = theme.font.name;
          background_color = "rgb(${faces.bgDefault})";
          focus_on_activate = true;
          new_window_takes_over_fullscreen = 1;
          exit_window_retains_fullscreen = true;
        };

        group = {
          groupbar = {
            font_size = toString theme.font.size;
          };
          "col.border_active" = "rgb(${faces.fgBorder})";
          "col.border_inactive" = "rgb(${faces.fgBorderInactive})";
          "col.border_locked_active" = "rgb(${faces.fgBorder})";
          "col.border_locked_inactive" = "rgb(${faces.fgBorderInactive})";
        };

        animations = {
          enabled = "true";
        };

        plugin = {
          scroller = {
            column_default_width = "onehalf";
            center_row_if_space_available = true;
            column_widths = "onefourth onethird onehalf twothirds threefourts one";
            cyclesize_wrap = false;
            jump_labels_keys = "isrtneao";
          };
        };
      };
      extraConfig = keybinder.binds;
      plugins = [ pkgs.hyprlandPlugins.hyprscroller ];
    };
  };
}
