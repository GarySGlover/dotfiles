{
  config,
  pkgs,
  ...
}:
let
  theme = config.wolf.theme;
  faces = theme.faces;
  element = theme.element;
  dracula = theme.dracula;
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
              fgColor = "#${faces.fgDefault}";
              bgColor = "#${faces.bgDefault}";
              logoColor = "#${dracula.purple}";
            };
            prompt = {
              fgColor = "#${faces.fgDefault}";
              bgColor = "#${faces.bgDefault}";
              suggestColor = "#${dracula.purple}";
            };
            info = {
              fgColor = "#${dracula.pink}";
              sectionColor = "#${faces.fgDefault}";
            };
            dialog = {
              fgColor = "#${faces.fgDefault}";
              bgColor = "#${faces.bgDefault}";
              buttonFgColor = "#${faces.fgDefault}";
              buttonBgColor = "#${dracula.purple}";
              buttonFocusFgColor = "#${dracula.yellow}";
              buttonFocusBgColor = "#${dracula.pink}";
              labelFgColor = "#${dracula.orange}";
              fieldFgColor = "#${faces.fgDefault}";
            };
            frame = {
              border = {
                fgColor = "#${faces.bgHighlight}";
                focusColor = "#${faces.bgHighlight}";
              };
              menu = {
                fgColor = "#${faces.fgDefault}";
                keyColor = "#${dracula.pink}";
                numKeyColor = "#${dracula.pink}";
              };
              crumbs = {
                fgColor = "#${faces.fgDefault}";
                bgColor = "#${faces.bgTabBarTabInactive}";
                activeColor = "#${faces.bgTabBarTab}";
              };
              status = {
                newColor = "#${dracula.cyan}";
                modifyColor = "#${dracula.purple}";
                addColor = "#${dracula.green}";
                errorColor = "#${dracula.red}";
                highlightColor = "#${dracula.orange}";
                killColor = "#${faces.fgFontLockCommentFace}";
                completedColor = "#${faces.fgFontLockCommentFace}";
              };
              title = {
                fgColor = "#${faces.fgDefault}";
                bgColor = "#${faces.bgHighlight}";
                highlightColor = "#${dracula.orange}";
                counterColor = "#${dracula.purple}";
                filterColor = "#${dracula.pink}";
              };
            };
            views = {
              charts = {
                bgColor = "default";
                defaultDialColors = [
                  "#${dracula.purple}"
                  "#${dracula.red}"
                ];
                defaultChartColors = [
                  "#${dracula.purple}"
                  "#${dracula.red}"
                ];
              };
              table = {
                fgColor = "#${faces.fgDefault}";
                bgColor = "#${faces.bgDefault}";
                header = {
                  fgColor = "#${faces.fgDefault}";
                  bgColor = "#${faces.bgDefault}";
                  sorterColor = "#${dracula.cyan}";
                };
              };
              xray = {
                fgColor = "#${faces.fgDefault}";
                bgColor = "#${faces.bgDefault}";
                cursorColor = "#${faces.bgHighlight}";
                graphicColor = "#${dracula.purple}";
                showIcons = false;
              };
              yaml = {
                keyColor = "#${faces.fgFontLockVariableNameFace}";
                colonColor = "#${faces.fgFontLockDelimiterFace}";
                valueColor = "#${faces.fgFontLockStringFace}";
              };
              logs = {
                fgColor = "#${faces.fgDefault}";
                bgColor = "#${faces.bgDefault}";
                indicator = {
                  fgColor = "#${faces.fgDefault}";
                  bgColor = "#${dracula.purple}";
                  toggleOnColor = "#${dracula.green}";
                  toggleOffColor = "#${dracula.cyan}";
                };
              };
            };
          };
        };
      };
    };
  };
}
