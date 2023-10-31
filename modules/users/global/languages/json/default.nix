{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  opt = config.wolf.languages.json;
in {
  options.wolf.languages.json.enable = mkOption {
    type = types.bool;
  };

  config = mkIf opt.enable {
    home.packages = with pkgs; [
      jq
    ];

    home.file.".jq".source = ./jqmodules;

    programs.fish.shellAliases = {
      jqs = "jq 'include \"schema\"; schema'";
    };
  };
}
