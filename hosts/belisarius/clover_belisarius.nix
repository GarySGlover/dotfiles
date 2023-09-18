{config, ...}: {
  imports = [
    ../../users/clover/home.nix
  ];

  home.file."${config.xdg.configHome}/hypr/host.conf".source = ./hypr.conf;
}
