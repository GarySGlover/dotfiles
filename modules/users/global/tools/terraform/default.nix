{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkIf;
in {
  config = mkIf config.wolf.roles.devops {
    home.packages = with pkgs; [
      checkov
      terraform
      terraform-docs
      terraform-ls
    ];
  };
}
