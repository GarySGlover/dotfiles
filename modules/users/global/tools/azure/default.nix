{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  opt = config.wolf.tools.azure;
in {
  options.wolf.tools.azure.enable = mkOption {
    type = types.bool;
  };

  config = mkIf opt.enable {
    home.packages = with pkgs; [
      azure-cli
    ];
  };
}
