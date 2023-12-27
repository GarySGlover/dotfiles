{
  lib,
  pkgs,
  config,
  ...
}:
with lib;
with types; let
  roles = config.wolf.system;
in {
  options.wolf.system = {
    physical = mkOption {
      type = bool;
      default = true;
    };
  };
}
