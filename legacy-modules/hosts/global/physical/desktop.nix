{
  lib,
  ...
}:
with lib;
{
  config = {
    # Firware update fix
    hardware.enableAllFirmware = true;
  };
}
