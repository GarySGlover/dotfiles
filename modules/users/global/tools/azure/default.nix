{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkIf isDerivation;
  inherit (builtins) filter attrValues;

  azure-cli = pkgs.azure-cli.withExtensions (filter (item: isDerivation item) (attrValues pkgs.azure-cli-extensions));
  packages = with pkgs; [
    kubelogin
  ];
in {
  config = mkIf config.wolf.roles.devops {
    home.packages = packages ++ [azure-cli];
  };
}
