{
  lib,
  config,
  pkgs,
  ...
}:
with builtins;
with lib;
let
  excludedExtensions = [
    "connection-monitor-preview"
    "blockchain"
    "aks-preview"
  ];

  isDesiredExtension =
    item:
    let
      name = item.pname or "";
      result = tryEval item;
    in
    result.success && !(elem name excludedExtensions) && isDerivation item;

  azure-cli = pkgs.azure-cli.withExtensions (
    filter isDesiredExtension (attrValues pkgs.azure-cli-extensions)
  );
  packages = with pkgs; [
    kubelogin
  ];
in
{
  config = mkIf (false && config.wolf.roles.devops) {
    home.packages = packages ++ [ azure-cli ];
  };
}
