{
  config,
  lib,
  pgks,
  ...
}:
with lib; let
  opt = config.wolf.tools.qmk;
in {
  options.wolf.tools.qmk.enable = mkOption {
    type = types.bool;
  };

  config = mfIf opt.enable {
    home.packages = with pkgs; [
      qmk
    ];
  };
}
