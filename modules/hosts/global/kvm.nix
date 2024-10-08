{
  lib,
  config,
  ...
}:
let
  inherit (lib) mkIf;
in
{
  config =
    mkIf (config.wolf.system.physical && config.wolf.system.kvm)
      {
	virtualisation.libvirtd.enable = true;
	programs.virt-manager.enable = true;
      };
}
