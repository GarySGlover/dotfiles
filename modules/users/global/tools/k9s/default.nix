{
  config,
  pkgs,
  ...
}:
let
  theme = config.wolf.theme;
  colors = theme.colors;
in
{
  config = {
    programs.k9s = {
      enable = true;
      package = pkgs.curl;
      settings = {
        k9s = {
          liveViewAutoRefresh = true;
          ui = {
            logoless = true;
            skin = "theme";
            reactive = true;
          };
        };
      };
      skins = {
        theme = {
          k9s = {
            body = {
              fgColor = "${colors.foreground}";
              bgColor = "${colors.background}";
              logoColor = "${colors.violet}";
            };
            prompt = {
              fgColor = "${colors.foreground}";
              bgColor = "${colors.background}";
              suggestColor = "${colors.violet}";
            };
            info = {
              fgColor = "${colors.magenta}";
              sectionColor = "${colors.foreground}";
            };
            dialog = {
              fgColor = "${colors.foreground}";
              bgColor = "${colors.background}";
              buttonFgColor = "${colors.foreground}";
              buttonBgColor = "${colors.backgroundViolet}";
              buttonFocusFgColor = "${colors.yellow}";
              buttonFocusBgColor = "${colors.backgroundMagenta}";
              labelFgColor = "${colors.orange}";
              fieldFgColor = "${colors.foreground}";
            };
            frame = {
              border = {
                fgColor = "${colors.backgroundGray}";
                focusColor = "${colors.backgroundGray}";
              };
              menu = {
                fgColor = "${colors.foreground}";
                keyColor = "${colors.magenta}";
                numKeyColor = "${colors.magenta}";
              };
              crumbs = {
                fgColor = "${colors.foreground}";
                bgColor = "#${colors.weak}";
                activeColor = "#${colors.background}";
              };
              status = {
                newColor = "${colors.cyan}";
                modifyColor = "${colors.violet}";
                addColor = "${colors.green}";
                errorColor = "${colors.red}";
                highlightColor = "${colors.orange}";
                killColor = "${colors.red}";
                completedColor = "${colors.blue}";
              };
              title = {
                fgColor = "${colors.foreground}";
                bgColor = "${colors.backgroundGray}";
                highlightColor = "${colors.orange}";
                counterColor = "${colors.violet}";
                filterColor = "${colors.magenta}";
              };
            };
            views = {
              charts = {
                bgColor = "default";
                defaultDialColors = [
                  "${colors.violet}"
                  "${colors.red}"
                ];
                defaultChartColors = [
                  "${colors.violet}"
                  "${colors.red}"
                ];
              };
              table = {
                fgColor = "${colors.foreground}";
                bgColor = "${colors.background}";
                header = {
                  fgColor = "${colors.foreground}";
                  bgColor = "${colors.background}";
                  sorterColor = "${colors.cyan}";
                };
              };
              xray = {
                fgColor = "${colors.foreground}";
                bgColor = "${colors.background}";
                cursorColor = "${colors.backgroundGray}";
                graphicColor = "${colors.violet}";
                showIcons = false;
              };
              yaml = {
                keyColor = "${colors.orange}";
                colonColor = "${colors.foreground}";
                valueColor = "#${colors.foreground}";
              };
              logs = {
                fgColor = "${colors.foreground}";
                bgColor = "${colors.background}";
                indicator = {
                  fgColor = "${colors.foreground}";
                  bgColor = "${colors.backgroundViolet}";
                  toggleOnColor = "${colors.green}";
                  toggleOffColor = "${colors.cyan}";
                };
              };
            };
          };
        };
      };
    };
  };
}
