{
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  opt = config.wolf;
in {
  config = mkIf (opt.roles.editing && opt.roles.devops) {
    programs.emacs.extraPackages = epkgs:
      with epkgs; [
        terraform-mode
        terraform-doc
      ];
  };
}
