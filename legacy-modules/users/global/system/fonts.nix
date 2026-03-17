{
  pkgs,
  ...
}:
{
  config = {
    # Needed to pickup fonts installed by home manager
    fonts.fontconfig.enable = true;

    home.packages = with pkgs; [
      nerd-fonts._0xproto
    ];
  };
}
