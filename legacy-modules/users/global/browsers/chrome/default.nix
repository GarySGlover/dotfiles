{
  pkgs,
  ...
}:
{
  config = {
    home.packages = with pkgs; [
      # chromium
      # google-chrome
    ];
  };
}
