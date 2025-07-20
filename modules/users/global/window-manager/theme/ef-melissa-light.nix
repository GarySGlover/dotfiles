{
  config,
  lib,
  ...
}:
{
  config.wolf.theme = lib.mkIf (config.wolf.theme.name == "ef-melissa-light") {
    type = "light";
    font = {
      name = "0xProto Nerd Font";
      size = 10;
    };

    border = 2;
    gaps = 2;

    colors.background = "#FFF6D8";

    colors_2 = {
      background = "#fff6d8";
      bg = "#ffde72";
      bg_alt = "#ffeaa5";
      bg_dim = "#ffe48c";
      blue = "#1f4ead";
      border_active = "#ffde72";
      border_inactive = "#ffeaa5";
      brown = "#945e38";
      cyan = "#1f87ad";
      fg = "#fff0be";
      fg_alt = "#fff6d8";
      fg_bright = "#ffeaa5";
      fg_dim = "#fff0be";
      fg_max = "#ffe48c";
      foreground = "#1f1c14";
      green = "#1fad45";
      orange = "#ad5d1f";
      purple = "#ad1f9f";
      red = "#ad2d1f";
      selection_bg = "#abff3f";
      selection_fg = "#1c1b17";
      yellow = "#e6c419";
    };
  };
}
