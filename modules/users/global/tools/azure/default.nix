{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkIf isDerivation;
  inherit (builtins) filter attrValues tryEval;

  excludedExtensions = [
    "connection-monitor-preview"
    "blockchain"
  ];

  isDesiredExtension =
    item:
    let
      name = item.pname or "";
      result = tryEval item;
    in
    result.success && !(lib.elem name excludedExtensions) && isDerivation item;

  azure-cli = pkgs.azure-cli.withExtensions (
    filter isDesiredExtension (attrValues pkgs.azure-cli-extensions)
  );
  packages = with pkgs; [
    kubelogin
  ];
in
{
  config = mkIf config.wolf.roles.devops {
    home.packages = packages ++ [ azure-cli ];
  };
}
