{
  config,
  lib,
  ...
}:
let
  inherit (lib) mkIf;
in
{
  config = mkIf config.wolf.system.physical {
    # Bluetooth enable
    hardware.bluetooth.enable = true;

    sops.secrets.wireless-env = { };

    networking.networkmanager.enable = false;
    networking.wireless = {
      enable = true;
      secretsFile = config.sops.secrets.wireless-env.path;
      networks = {
        CLOVER.pskRaw = "ext:clover";
      };
    };
  };
}
