{
  pkgs,
  config,
  lib,
  ...
}:
with lib; {
  config = mkIf config.wolf.roles.editing {
    programs.emacs.extraPackages = epkgs:
      with epkgs; [
        avy
        consult
        consult-flycheck
        corfu
        embark
        embark-consult
        flycheck-posframe
        marginalia
        nerd-icons-completion
        orderless
        vertico
        vertico-posframe
      ];
  };
}
