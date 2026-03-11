# [[file:../../modules.org::*Direnv][Direnv:1]]
{
  flake.aspects.direnv.homeManager = {
    programs.direnv = {
      enable = true;
      enableBashIntegration = true;
      nix-direnv.enable = true;
    };

    programs.starship.settings.direnv.disabled = false;

    home.sessionVariables = {
      DIRENV_LOG_FORMAT = "";
    };
  };
}
# Direnv:1 ends here
