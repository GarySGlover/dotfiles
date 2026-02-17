{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
{
  config = {
    # Firware update fix
    hardware.enableAllFirmware = true;
  };
}
