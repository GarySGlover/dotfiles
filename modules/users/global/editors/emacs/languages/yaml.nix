{
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  opt = config.wolf;
in {
  config = mkIf (opt.roles.editing && opt.roles.programming) {
    home.packages = with pkgs; [
      yamlfmt
      yamllint
    ];
  };
}
