{
  config,
  lib,
  ...
}:
with lib; {
  options.wolf = {
    terminal.kitty = {
      enable = mkOption {
        type = types.bool;
      };
    };
  };

  config = mkIf config.wolf.terminal.kitty.enable {
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
