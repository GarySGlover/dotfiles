{
  config,
  lib,
  ...
}:
let
  inherit (lib) mkIf;
  theme = config.wolf.theme;
  colors = theme.colors;
in
{
  config = mkIf config.wolf.roles.desktop {
    programs.kitty = {
      enable = true;
      font = theme.font;
      shellIntegration = {
        enableFishIntegration = true;
        enableBashIntegration = true;
      };
      extraConfig = ''
        foreground            ${colors.foreground}
        background            ${colors.background}
        selection_foreground  ${colors.strong}
        selection_background  ${colors.backgroundGray}

        url_color ${colors.blue}

        # black
        color0  #000000
        color8  ${colors.darkGray}

        # red
        color1  ${colors.red}
        color9  ${colors.strongRed}

        # green
        color2  ${colors.green}
        color10 ${colors.strongGreen}

        # yellow
        color3  ${colors.yellow}
        color11 ${colors.strongYellow}

        # blue
        color4  ${colors.blue}
        color12 ${colors.strongBlue}

        # magenta
        color5  ${colors.magenta}
        color13 ${colors.strongMagenta}

        # cyan
        color6  ${colors.cyan}
        color14 ${colors.strongCyan}

        # white
        color7  #FFFFFF
        color15 ${colors.lightGray}

        # Tab bar colours
        active_tab_foreground   ${colors.strong}
        active_tab_background   ${colors.weak}
        inactive_tab_foreground ${colors.foreground}
        inactive_tab_background ${colors.background}

        # Splits/Windows
        active_border_color ${colors.strong}
        inactive_border_color ${colors.weak}
        background_opacity 1.0
      '';
    };
  };
}
