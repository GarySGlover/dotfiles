{
  config,
  pkgs,
  lib,
  ...
}:
let
  inherit (lib) mkIf;
in
{
  config = {
    home.file."${config.xdg.configHome}/black".text = ''
      [tool.black]
      line-length=80
    '';

    home.sessionPath = [
      "$HOME/.local/bin"
    ];
  };
}
