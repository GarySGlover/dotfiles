{
  config,
  pkgs,
  ...
}: {
  home.file."${config.xdg.configHome}/black" = {
    text = ''
      [tool.black]
      line-length=80
    '';
  };
  home.packages = with pkgs; [
    nodePackages.pyright
    python311
    python311Packages.black
    python311Packages.flake8
  ];
}
