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
  };
}
