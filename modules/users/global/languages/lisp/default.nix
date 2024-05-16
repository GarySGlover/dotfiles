{
  config,
  pkgs,
  lib,
  ...
}:
with lib; {
  config = mkIf config.wolf.languages.lisp {
    home.packages = with pkgs; [
      sbcl
    ];
  };
}
