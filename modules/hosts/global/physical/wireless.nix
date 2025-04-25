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
        CLOVER = {
          pskRaw = "ext:clover";
          extraConfig = ''
            # Force frequency band to 5GHz in the United Kingdom
            freq_list="5160 5180 5200 5220 5240 5260 5280 5300 5320 5340 5480 5500 5520 5540 5560 5580 5600 5620 5640 5660 5680 5700 5720 5745 5765 5785 5805 5825 5845 5865 5885"
          '';
        };
      };
    };
  };
}
