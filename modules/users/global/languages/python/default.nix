{
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (lib) mkIf;
in {
  config = mkIf config.wolf.languages.python {
    home.file."${config.xdg.configHome}/black".text = ''
      [tool.black]
      line-length=80
    '';

    home.packages = with pkgs; [
      python312
      python312Packages.black
      python312Packages.flake8
      python312Packages.pipx
      python312Packages.pip
      pyright
    ];

    home.sessionPath = [
      "$HOME/.local/bin"
    ];
  };
}
