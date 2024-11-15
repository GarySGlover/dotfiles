{
  config,
  lib,
  ...
}:
with lib;
let
  wolf = config.wolf;
in
{
  config = mkIf wolf.roles.desktop {
    services.dunst.enable = true;
  };
}
