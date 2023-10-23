{
  pkgs,
  config,
  lib,
  ...
}:
with lib; {
  config = mkIf config.wolf.editors.emacs.enable {
    programs.emacs.extraPackages = epkgs:
      with epkgs; [
        consult
        corfu
        orderless
        vertico
        vertico-posframe
      ];
  };
}
