{
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  opt = config.wolf;
in {
  config = mkIf opt.roles.editing {
    programs.emacs.extraPackages = epkgs:
      with epkgs; [];
  };
}
