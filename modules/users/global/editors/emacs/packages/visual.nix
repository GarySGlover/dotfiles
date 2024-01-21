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
        breadcrumb
        doom-modeline
        doom-themes
        highlight-indent-guides
        rainbow-delimiters
      ];
  };
}
