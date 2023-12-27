{
  config,
  lib,
  ...
}:
with lib; {
  config = mkIf config.wolf.system.physical {
    # Bluetooth enable
    hardware.bluetooth.enable = true;

    sops.secrets.wireless-env = {};

    networking.networkmanager.enable = false;
    networking.wireless = {
      enable = true;
      environmentFile = config.sops.secrets.wireless-env.path;
      networks = {
        "@home_uid@" = {
          pskRaw = "@home_psk@";
        };
      };
    };
  };
}
