{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (builtins) hasAttr;
  inherit (lib) mkIf forEach;
  secrets = import "${config.wolf.secretsPath}/${config.home.username}-secrets.nix";
in
{
  config = {
    xdg.configFile = {
      "git/templates/hooks/pre-commit" = {
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
      "git/templates/hooks/post-clone" = {
        text = ''
          #!/bin/bash

          # Change to the directory of the cloned repository
          cd "$1"

          # Check if it's a bare repository
          if [ -d ".git" ]; then
              echo "This is not a bare repository. Exiting..."
              exit 0
          fi

          # Delete all local branches except master/main
          git for-each-ref --format='%(refname:short)' refs/heads | xargs -n 1 git branch -D

        '';
        executable = true;
      };
    };

    home.packages = with pkgs; [
      pre-commit
    ];

    programs.git = {
      enable = true;
      userName = "${secrets.git_username}";
      extraConfig = {
        core.askPass = "";
        credential.helper = "store";
        fetch = {
          prune = true;
          pruneTags = true;
        };
        github = mkIf (hasAttr "github_user" secrets) {
          user = "${secrets.github_user}";
        };
        init = {
          templateDir = "${config.xdg.configHome}/git/templates";
        };
      };
      aliases = {
        fetchp = "fetch --force";
      };
      includes =
        forEach secrets.git_remotes_emails (x: {
          condition = "hasconfig:remote.*.url:${x.condition}";
          contents = {
            user = {
              email = "${x.email}";
            };
          };
        })
        ++ forEach secrets.git_folders_emails (x: {
          condition = "gitdir:${x.condition}";
          contents = {
            user = {
              email = "${x.email}";
            };
          };
        });
    };
  };
}
