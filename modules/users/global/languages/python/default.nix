{
  config,
  pkgs,
  lib,
  ...
}:
with lib; let
  opt = config.wolf.languages.python;
in {
  options.wolf = {
    languages = {
      python = {
        package = mkOption {
          default = pkgs.python311;
        };
        enable = mkOption {
          type = types.bool;
        };
        useCodeStyle = mkOption {
          type = types.bool;
          default = true;
        };
      };
    };
  };
  config = mkIf opt.enable {
    home.file = {
      ${
        if opt.useCodeStyle
        then "${config.xdg.configHome}/black"
        else "null"
      } = {
        text = ''
          [tool.black]
          line-length=80
        '';
      };
    };

    home.packages = with pkgs;
      [
        python311
      ]
      ++ (
        if opt.useCodeStyle
        then [
          nodePackages.pyright
          python311Packages.black
          python311Packages.flake8
        ]
        else []
      );
  };
}
