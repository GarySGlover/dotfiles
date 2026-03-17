{
  pkgs,
  ...
}:
{
  config = {
    home.packages = with pkgs; [
      brave
    ];
  };
}
