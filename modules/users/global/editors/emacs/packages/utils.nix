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
        direnv
        gcmh
        exec-path-from-shell
        magit
        mini-frame
        no-littering
        transient
        (callPackage ./manual/combobulate.nix {
          inherit (pkgs) fetchFromGitHub writeText;
          inherit (epkgs) melpaBuild compat;
        })
      ];
  };
}
