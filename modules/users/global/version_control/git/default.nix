{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  secrets = import "${config.wolf.secretsPath}/${config.home.username}-secrets.nix";
  opt = config.wolf.git.precommit;
in {
  options.wolf = {
    git.precommit = {
      enable = mkOption {
        type = types.bool;
      };
      autoInstall = mkOption {
        description = "Auto install pre commit hooks for repos";
        type = types.bool;
        default = true;
      };
    };
  };

  config = mkMerge [
    (mkIf (opt.enable && opt.autoInstall)
      {
        xdg.configFile."git/templates/hooks/pre-commit".text = ''
          #!/usr/bin/env bash
          # start templated
          INSTALL_PYTHON=/usr/bin/env python3
          ARGS=(hook-impl --config=.pre-commit-config.yaml --hook-type=pre-commit --skip-on-missing-config)
          # end templated

          HERE="$(cd "$(dirname "$0")" && pwd)"
          ARGS+=(--hook-dir "$HERE" -- "$@")

          exec /usr/bin/env pre-commit "''${ARGS[@]}"
        '';
      })
    (
      mkIf opt.enable {
        home.packages = with pkgs; [
          config.wolf.languages.python.package
          pre-commit
          go
        ];
      }
    )
    {
      programs.git = {
        enable = true;
        userName = "${secrets.git_username}";
        extraConfig = {
          init = mkIf (config.wolf.git.precommit.enable && config.wolf.git.precommit.autoInstall) {
            templateDir = "${config.xdg.configHome}/git/templates/hooks/pre-commit";
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
    }
  ];
}
