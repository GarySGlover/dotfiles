{ pkgs, ... }:
let
  local-pkgs = import ../../../../../packages/packages.nix { inherit pkgs; };
in
{
  config = {
    programs.direnv = {
      enable = true;
      enableBashIntegration = true;
      nix-direnv.enable = true;
    };

    programs.starship.settings.direnv.disabled = false;

    home.sessionVariables = {
      DIRENV_LOG_FORMAT = "";
    };

    home.packages = with local-pkgs; [ e ];
  };
}
