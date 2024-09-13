{
  lib,
  config,
  ...
}:
let
  inherit (lib) mkIf;
in
{
  config =
    mkIf config.wolf.system.physical
      {
      };
}
