let
  secrets = import ../../secrets/clover-secrets.nix;
  homeDir = "/home/clover";
in {
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

  programs.git = {
    enable = true;
    userName = "${secrets.git_username}";
    extraConfig = {
      init = {
	templateDir = "${homeDir}/git/templates/hooks/pre-commit";
      };
    };
    includes = [
      {
        condition = "hasconfig:remote.*.url:${secrets.repo1.condition}";
        contents = {
          user = {
            email = "${secrets.repo1.email}";
          };
	};
      }
    ];
  };
}
