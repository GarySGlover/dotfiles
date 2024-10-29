{
  config,
  pkgs,
  lib,
  ...
}:
with lib;
let
  wolf = config.wolf;
  theme = wolf.theme;
  faces = theme.faces;
in
{
  config = mkIf wolf.roles.desktop {
    programs.fuzzel = {
      enable = true;
      settings = {
        main = {
          font = "${theme.font.name}:size=${toString theme.font.size}";
          horizontal-pad = "${toString theme.font.size}";
          vertical-pad = "${toString theme.font.size}";
          dpi-aware = "no";
        };
        colors = {
          background = "${faces.bgDefault}FF";
          text = "${faces.fgDefault}FF";
          prompt = "${faces.fgMinibufferPrompt}FF";
          input = "${faces.fgDefault}FF";
          match = "${faces.fgOrderlessMatchFace0}FF";
          selection = "${faces.bgRegion}FF";
          selection-text = "${faces.fgRegion}FF";
          selection-match = "${faces.fgOrderlessMatchFace0}FF";
          counter = "${faces.fgDefault}FF";
          border = "${faces.fgBorder}FF";
        };
        border = {
          width = "${toString theme.border}";
        };
      };
    };
  };
}
