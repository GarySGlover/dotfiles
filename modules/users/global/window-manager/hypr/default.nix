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
      inherit lib;
      inherit config;
      roles = config.wolf.roles;
    }
  );
in
{
  config = mkIf config.wolf.roles.desktop {
    home.file."${config.xdg.configHome}/hypr/devil.conf".text =
      (import ./devil.nix {
        inherit lib;
        inherit config;
        roles = config.wolf.roles;
      }).binds;
    home.file."${config.xdg.configHome}/hypr/hypr-insert.sh".source = ./hypr-insert.sh;
    home.packages = with pkgs; [ libnotify ];

    wayland.windowManager.hyprland = {
      enable = true;
      settings = {
        workspace = builtins.map (w: "${w.id}, defaultName:${w.name}") keybinder.workSpaces;
        windowrulev2 = [
          "workspace special:room101 silent, title:^(meet.google.com is sharing a window.)$"
          "float, initialTitle:(MainPicker)"
          "bordercolor rgb(FF0000) rgb(880808), fullscreen:1"
        ] ++ (if config.wolf.roles.gaming then [ "workspace 10,class:(steam)" ] else [ ]);

        exec-once = [
          "${pkgs.hyprland}/bin/hyprctl dispatch submap insert" # Start in the modal command mappings
          "kanshi" # Monitor management daemon
          "udiskie &" # Disk auto mount
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
          layout = "master";
          border_size = theme.border;
          gaps_in = toString theme.font.size;
          gaps_out = toString theme.font.size;
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
          drop_shadow = false;
          rounding = toString theme.radius;
        };

        misc = {
          disable_hyprland_logo = true;
          font_family = theme.font.name;
          background_color = "rgb(${faces.bgDefault})";
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
          enabled = "false";
        };
      };
      extraConfig = keybinder.binds;
    };
  };
}
