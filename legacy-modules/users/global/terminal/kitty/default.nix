{
  config,
  lib,
  ...
}:
let
  inherit (lib) mkIf;
in
{
  config = {
    programs.kitty = {
      enable = true;
      shellIntegration = {
        enableFishIntegration = true;
        enableBashIntegration = true;
      };
    };
  };
}
