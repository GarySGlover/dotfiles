{
  pkgs,
  config,
  lib,
  ...
}:
with lib; {
  config = mkIf (config.wolf.roles.editing && config.wolf.roles.devops) {
    programs.emacs.extraPackages = epkgs:
      with epkgs; [
        kele
      ];
  };
}
