{ lib, ... }:
let
  inherit (lib) mkOption types;
  inherit (types) bool;
in
{
  options.wolf.system = {
    physical = mkOption {
      type = bool;
      default = true;
    };
  };
}
