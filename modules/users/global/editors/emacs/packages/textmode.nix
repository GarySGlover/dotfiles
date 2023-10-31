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
        visual-fill-column
      ];
  };
}
