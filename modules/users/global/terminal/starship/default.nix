{
  config,
  lib,
  ...
}:
with lib; {
  config = {
    programs.starship = {
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = true;
      enableFishIntegration = true;
    };
  };
}
