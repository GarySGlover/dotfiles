# useful ref for amdgpu https://www.reddit.com/r/NixOS/comments/1mq7qni/chasing_the_perfect_amd_gpu_config_module/
# amd gpu crash https://discourse.nixos.org/t/yet-another-gcvm-l2-protection-fault-status-problem/65420
{ pkgs, ... }:
{
  # ASUS Services
  services.asusd = {
    enable = true;
    enableUserService = true;
  };

  hardware = {
    graphics = {
      enable = true;
      enable32Bit = true;
      extraPackages = with pkgs; [ rocmPackages.clr.icd ];
    };
  };

  services.lact.enable = true;

  systemd.tmpfiles.rules =
    let
      rocmEnv = pkgs.symlinkJoin {
        name = "opt-rocm";
        paths = with pkgs.rocmPackages; [
          rocblas
          hipblas
          clr
        ];
      };
    in
    [
      "L+    /opt/rocm   -    -    -     -    ${rocmEnv}"
    ];

  environment.systemPackages = with pkgs; [
    clinfo
    mesa-demos
    vulkan-tools
    amdgpu_top
    rocmPackages.rocm-smi
  ];

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
}
