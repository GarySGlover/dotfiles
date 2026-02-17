# [[file:../modules.org::*Azure][Azure:2]]
{
  inputs,
  lib,
  ...
}:
{
  flake.aspects = {
    options = {
      homeManager =
        { config, lib, ... }:
        {
          options.azure-cli.extensions = lib.mkOption {
            type = lib.types.listOf lib.types.package // {
              merge = loc: defs: lib.concatLists (map (def: def.value) defs);
            };
            default = [ ];
            description = "List of Azure CLI extension packages.";
            example = ''
              with pkgs.azure-cli.extensions; [
                azure-devops
              ];
            '';
          };
        };
    };
    azure = {
      homeManager =
        {
          inputs,
          pkgs,
          config,
          ...
        }:
        {
          config = {
            kubernetes.includeKubelogin = true;
            azure-cli.extensions = with pkgs.azure-cli.extensions; [
              azure-devops
            ];
            home.packages = [
              (inputs.self.packages.${pkgs.stdenv.hostPlatform.system}.azure-cli.withExtensions
                config.azure-cli.extensions
              )
            ];
          };
        };
    };
  };
  perSystem =
    { pkgs, ... }:
    {
      packages = {
        azure-cli = inputs.wrappers.lib.wrapPackage {
          inherit pkgs;
          env = {
            AZURE_CORE_ONLY_SHOW_ERRORS = "true";
            AUZRE_CORE_COLLECT_TELEMETRY = "false";
          };
          runtimeInputs = [ pkgs.bkt ];
          package = pkgs.azure-cli;
          preHook = ''
            if [ $# -ge 2 ] && [ "$1" = "account" ] && [ "$2" = "get-access-token" ]; then
            	bkt --ttl=60m --stale=10s --discard-failures -- "$real_program" "$@"
            	exit $?
            fi
          '';
        };
      };
    };
}
# Azure:2 ends here
