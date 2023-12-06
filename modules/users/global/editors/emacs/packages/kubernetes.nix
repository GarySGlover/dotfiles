{
  pkgs,
  config,
  lib,
  ...
}:
with lib; {
  config = mkIf (config.wolf.roles.editing && config.wolf.roles.devops) {
    programs.emacs.extraPackages = epkgs:
      with epkgs; [
        kele
      ];

    home.sessionVariables = {WOLF_TOOLS_K8S = "true";};

    programs.emacs.extraConfig = ''
      (exec-path-from-shell-copy-env "WOLF_TOOLS_K8S")
    '';
  };
}
