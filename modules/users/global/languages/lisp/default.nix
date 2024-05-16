{
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (lib) mkIf;
in {
  config = mkIf config.wolf.languages.lisp {
    home.packages = with pkgs; [
      sbcl
    ];
  };
}
