{
  config,
  pkgs,
  lib,
  ...
}:
with lib; {
  config = mkIf config.wolf.languages.powershell {
    home.packages = with pkgs; [
      powershell
    ];
  };
}
