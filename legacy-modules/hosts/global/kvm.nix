{
  lib,
  config,
  ...
}:
let
  inherit (lib) mkIf;
in
{
  config = {
    virtualisation.libvirtd.enable = true;
    programs.virt-manager.enable = true;
  };
}
