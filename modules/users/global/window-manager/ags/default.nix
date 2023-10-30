{
  pkgs,
  config,
  lib,
  ...
}:
with lib; {
  options.wolf = {
    window-manager.ags = {
      enable = mkOption {
        type = types.bool;
      };
    };
  };

  config = mkIf config.wolf.window-manager.ags.enable {
    home.packages = with pkgs; [
      ags
    ];
  };
}
