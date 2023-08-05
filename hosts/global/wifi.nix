{ config, ...}:

{
  sops.secrets.wireless-env = {};

  networking.wireless = {
    enable = true;
    environmentFile = config.sops.secrets.wireless-env.path;
    networks = {
      "@home_uid@" = {
        pskRaw = "@home_psk@";
      };
    };
  };
}
