{
  config,
  lib,
  ...
}:
let
  inherit (lib) mkIf;
  theme = config.wolf.theme;
  faces = theme.faces;
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
        foreground            #${faces.fgDefault}
        background            #${faces.bgDefault}
        selection_foreground  #${faces.fgHighlight}
        selection_background  #${faces.bgHighlight}

        url_color #${faces.fgLink}

        # black
        color0  #${faces.fgAnsiColorBlack}
        color8  #${faces.fgAnsiColorBrightBlack}

        # red
        color1  #${faces.fgAnsiColorRed}
        color9  #${faces.fgAnsiColorBrightRed}

        # green
        color2  #${faces.fgAnsiColorGreen}
        color10 #${faces.fgAnsiColorBrightGreen}

        # yellow
        color3  #${faces.fgAnsiColorYellow}
        color11 #${faces.fgAnsiColorBrightYellow}

        # blue
        color4  #${faces.fgAnsiColorBlue}
        color12 #${faces.fgAnsiColorBrightBlue}

        # magenta
        color5  #${faces.fgAnsiColorMagenta}
        color13 #${faces.fgAnsiColorBrightMagenta}

        # cyan
        color6  #${faces.fgAnsiColorCyan}
        color14 #${faces.fgAnsiColorBrightCyan}

        # white
        color7  #${faces.fgAnsiColorWhite}
        color15 #${faces.fgAnsiColorBrightWhite}

        # Cursor colours
        cursor            #${faces.fgCursor}
        cursor_text_color #${faces.fgCursor}

        # Tab bar colours
        active_tab_foreground   #${faces.fgTabBarTab}
        active_tab_background   #${faces.bgTabBarTab}
        inactive_tab_foreground #${faces.fgTabBarTabInactive}
        inactive_tab_background #${faces.bgTabBarTabInactive}

        # Splits/Windows
        active_border_color #${faces.fgBorder}
        inactive_border_color #${faces.fgBorderInactive}
        background_opacity 1.0
      '';
    };
  };
}
