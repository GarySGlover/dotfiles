{
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  opt = config.wolf;
in {
  config = mkIf opt.roles.editing {
    programs.emacs.extraPackages = epkgs:
      with epkgs; [
        flymake-popon
        sideline
        sideline-flymake
        sideline-blame
        (callPackage ./manual/sideline-eglot.nix {
          inherit (pkgs) fetchFromGitHub writeText;
          inherit (epkgs) melpaBuild sideline ht;
        })
      ];
  };
}
