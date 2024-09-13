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
  config = mkIf config.wolf.languages.powershell {
    home.packages = with pkgs; [
      powershell
    ];
  };
}
