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
  };
}
