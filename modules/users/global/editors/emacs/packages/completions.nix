{
  pkgs,
  config,
  lib,
  ...
}:
with lib; {
  config = mkIf config.wolf.editors.emacs.enable {
    programs.emacs.extraPackages = epkgs:
      with epkgs; [
        avy
        consult
        corfu
        embark
        marginalia
        nerd-icons-completion
        orderless
        vertico
        vertico-posframe
      ];
  };
}
