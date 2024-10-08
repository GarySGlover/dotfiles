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
    docker = mkOption {
      type = bool;
      default = false;
    };
    kvm = mkOption {
      type = bool;
      default = false;
    };
  };
}
