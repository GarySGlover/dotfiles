{ pkgs, ... }:
{
  config = {
    home.packages = with pkgs; [ wl-clipboard ];
  };
}
