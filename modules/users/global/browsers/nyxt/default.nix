{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  opt = config.wolf.browsers.nyxt;
in {
  options.wolf.browsers.nyxt.enable = mkOption {
    type = types.bool;
  };

  config = mkIf opt.enable {
    home.packages = with pkgs; [
      nyxt
    ];
  };
}
