{
  pkgs,
  ...
}:
{
  config = {
    home.packages = with pkgs; [
      shellcheck
    ];
  };
}
