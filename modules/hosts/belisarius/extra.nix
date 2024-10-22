{ config, ... }:
{
  # ASUS Services
  services.asusd.enable = true;

  hardware.graphics.enable = true;
  hardware.graphics.enable32Bit = true;

  hardware.steam-hardware.enable = true;

  users.groups.realtime = { };
  services.udev.extraRules = ''
    KERNEL=="cpu_dma_latency", GROUP="realtime"
  '';
  security.pam.loginLimits = [
    {
      domain = "@realtime";
      type = "-";
      item = "rtprio";
      value = 98;
    }
    {
      domain = "@realtime";
      type = "-";
      item = "memlock";
      value = "unlimited";
    }
    {
      domain = "@realtime";
      type = "-";
      item = "nice";
      value = -11;
    }
  ];

  # OBS Studio
  boot.extraModulePackages = with config.boot.kernelPackages; [
    v4l2loopback
  ];
  boot.extraModprobeConfig = ''
    options v4l2loopback devices=1 video_nr=1 card_label="OBS Cam" exclusive_caps=1
  '';
  security.polkit.enable = true;
}
