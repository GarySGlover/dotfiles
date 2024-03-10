{
  lib,
  config,
  ...
}:
with lib; {
  config =
    mkIf config.wolf.system.physical {
    };
}
