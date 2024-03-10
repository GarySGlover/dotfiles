{
  pkgs,
  config,
  lib,
  writeText,
  ...
}:
with lib; {
  config = mkIf config.wolf.roles.editing {
    programs.emacs.extraPackages = epkgs:
      with epkgs; [
        breadcrumb
        ef-themes
        doom-modeline
        rainbow-delimiters
        (callPackage ./manual/indent-bars.nix {
          inherit (pkgs) fetchFromGitHub writeText;
          inherit (epkgs) melpaBuild compat;
        })
      ];
  };
}
