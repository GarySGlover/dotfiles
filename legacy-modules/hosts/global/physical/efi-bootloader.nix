{
  config,
  lib,
  ...
}:
let
  inherit (lib) mkIf;
in
{
  config = {
    boot.loader.systemd-boot.enable = true;
    boot.loader.systemd-boot.editor = false;
    boot.loader.efi.canTouchEfiVariables = true;
  };
}
