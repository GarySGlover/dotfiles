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
      python311Packages.black
      python311Packages.flake8
      python311Packages.pipx
      python311Packages.pip
      nodePackages.pyright
    ];

    home.sessionPath = [
      "$HOME/.local/bin"
    ];
  };
}
