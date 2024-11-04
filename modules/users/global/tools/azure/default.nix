{
  lib,
  config,
  pkgs,
  ...
}:
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
      result = lib.tryEval item;
    in
    result.success && !(builtins.elem name excludedExtensions) && lib.isDerivation item;

  azure-cli = pkgs.azure-cli.withExtensions (
    builtins.filter isDesiredExtension (lib.attrValues pkgs.azure-cli-extensions)
  );
  packages = with pkgs; [
    kubelogin
  ];
in
{
  config = lib.mkIf (false && config.wolf.roles.devops) {
    home.packages = packages ++ [ azure-cli ];
  };
}
