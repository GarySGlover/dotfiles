{
  config,
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [ kanata ];
  home.file."${config.xdg.configHome}/kanata/kanata.kbd".source = ./kanata.kbd;
}
