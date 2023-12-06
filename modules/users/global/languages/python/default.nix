{
  config,
  pkgs,
  lib,
  ...
}:
with lib; {
  config = mkIf config.wolf.languages.python {
    home.file."${config.xdg.configHome}/black".text = ''
      [tool.black]
      line-length=80
    '';

    home.packages = with pkgs; [
      python311
      nodePackages.pyright
      python311Packages.black
      python311Packages.flake8
      python311Packages.pipx
    ];

    home.sessionPath = [
      "$HOME/.local/bin"
    ];
  };
}
