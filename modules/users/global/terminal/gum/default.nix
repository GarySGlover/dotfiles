{ pkgs, ... }:
{
  config = {
    home.packages = with pkgs; [
      gum
      skate
    ];
  };
}
