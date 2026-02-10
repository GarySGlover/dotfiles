{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkIf;
in
{
  config = {
    home.packages = with pkgs; [
      jq
      jqp
    ];

    home.file.".jq".source = ./jqmodules;

    programs.fish.shellAliases = {
      jqs = "jq 'include \"schema\"; schema'";
    };
  };
}
