{
  pkgs,
  config,
  lib,
  ...
}:
with lib; {
  config = mkIf config.wolf.roles.editing {
    programs.emacs.extraPackages = epkgs:
      with epkgs; [
        direnv
        exec-path-from-shell
        magit
        mini-frame
        no-littering
        posframe
        transient
        transient-posframe
      ];
  };
}
