{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  roles = config.wolf.roles;
in {
  config = mkIf roles.azure_devops {
    home.packages = with pkgs; [
      checkov
      terraform
      terraform-docs
    ];
  };
}
