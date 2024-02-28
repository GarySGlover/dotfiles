{
  pkgs,
  config,
  lib,
  ...
}:
with lib;
with builtins; let
  secrets = import "${config.wolf.secretsPath}/${config.home.username}-secrets.nix";
in {
  config = mkIf config.wolf.roles.editing {
    programs.emacs.extraPackages = epkgs:
      with epkgs; [
        gptel
      ];

    home.file.authinfo = {
      target = ".authinfo";
      text =
        if (hasAttr "openai_token" secrets)
        then ''
          machine api.openai.com login apikey password ${secrets.openai_token}
        ''
        else "";
    };
  };
}
