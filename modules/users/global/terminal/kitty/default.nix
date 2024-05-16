{
  config,
  lib,
  ...
}: let
  inherit (lib) mkIf;
in {
  config = mkIf config.wolf.roles.desktop {
    programs.kitty = {
      enable = true;
      font = {
        name = "FiraCode Nerd Font";
        size = 10;
      };
      shellIntegration.enableFishIntegration = true;
      extraConfig = ''
        background_opacity 0.8
      '';
      theme = "Dracula";
    };
  };
}
