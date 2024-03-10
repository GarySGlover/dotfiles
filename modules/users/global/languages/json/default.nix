{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
in {
  config = mkIf config.wolf.languages.json {
    home.packages = with pkgs; [
      jq
    ];

    home.file.".jq".source = ./jqmodules;

    programs.fish.shellAliases = {
      jqs = "jq 'include \"schema\"; schema'";
    };
  };
}
