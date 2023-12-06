{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  secrets = import "${config.wolf.secretsPath}/${config.home.username}-secrets.nix";
in {
  config = {
    xdg.configFile."git/templates/hooks/pre-commit" = {
      text = ''
        #!/usr/bin/env bash
        # start templated
        INSTALL_PYTHON=python3
        ARGS=(hook-impl --config=.pre-commit-config.yaml --hook-type=pre-commit --skip-on-missing-config)
        # end templated

        HERE="$(cd "$(dirname "$0")" && pwd)"
        ARGS+=(--hook-dir "$HERE" -- "$@")

        exec /usr/bin/env pre-commit "''${ARGS[@]}"
      '';
      executable = true;
    };

    home.packages = with pkgs; [
      python311
      pre-commit
      go
    ];

    programs.git = {
      enable = true;
      userName = "${secrets.git_username}";
      extraConfig = {
        init = {
          templateDir = "${config.xdg.configHome}/git/templates";
        };
        github = {
          user = "${secrets.github_user}";
        };
      };
      includes = lists.forEach secrets.repos_emails (x: {
        condition = "hasconfig:remote.*.url:${x.condition}";
        contents = {
          user = {
            email = "${x.email}";
          };
        };
      });
    };
  };
}
