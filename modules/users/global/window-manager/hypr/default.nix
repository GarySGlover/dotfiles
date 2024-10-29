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
in
{
  imports = [
    ./keybinds.nix
  ];

  config = mkIf config.wolf.roles.desktop {
    wayland.windowManager.hyprland = {
      enable = true;
      settings = {
        windowrulev2 = [
          "workspace 10,class:(steam)"
        ];

        exec-once = [
          "waybar"
          "kanshi" # Monitor management daemon
        ];
        exec = [
          "kanshictl reload" # Force monitor refresh on reload
        ];

        general = {
          layout = "hy3";
          border_size = theme.border;
          gaps_in = toString theme.font.size;
          gaps_out = toString theme.font.size;
          "col.inactive_border" = "rgb(${faces.fgBorderInactive})";
          "col.active_border" = "rgb(${faces.fgBorder})";
          "col.nogroup_border" = "rgb(${faces.fgBorderInactive})";
          "col.nogroup_border_active" = "rgb(${faces.fgBorder})";
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
          rounding = toString theme.font.size;
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

        plugin = {
          hy3 = {
            tab_first_window = 1;
            tabs = {
              height = toString (theme.font.size + (theme.border * 2));
              padding = theme.border;
              rounding = toString theme.font.size;
              text_font = theme.font.name;
              text_height = toString theme.font.size;
              text_padding = theme.border;
              "col.active" = "rgb(${faces.bgTabBarTab})";
              "col.text.active" = "rgb(${faces.fgTabBarTab})";
              "col.urgent" = "rgb(${faces.bgHighlight})";
              "col.text.urgent" = "rgb(${faces.fgHighlight})";
              "col.inactive" = "rgb(${faces.bgTabBarTabInactive})";
              "col.text.inactive" = "rgb(${faces.fgTabBarTabInactive})";
            };
          };
        };
      };
      extraConfig = builtins.readFile ./hyprland.conf;
      plugins = [ pkgs.hyprlandPlugins.hy3 ];
    };
  };
}
