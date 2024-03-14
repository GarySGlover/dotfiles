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
        consult-flyspell
        dtrt-indent
        format-all
      ];

    home.packages = with pkgs; [
      (aspellWithDicts (ds: with ds; [en en-computers en-science]))
    ];
  };
}
