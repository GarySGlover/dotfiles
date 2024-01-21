{
  config,
  lib,
  pkgs,
  ...
}: {
  config = {
    programs.direnv = {
      enable = true;
      enableBashIntegration = true;
      nix-direnv.enable = true;
    };
  };
}
