{
  config,
  lib,
  ...
}:
with lib; {
  config = mkIf config.wolf.system.physical {
    boot.loader.systemd-boot.enable = true;
    boot.loader.systemd-boot.editor = false;
    boot.loader.efi.canTouchEfiVariables = true;
  };
}
