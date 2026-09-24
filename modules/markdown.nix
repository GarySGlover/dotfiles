# [[file:../modules.org::*Markdown][Markdown:1]]
{
  flake.aspects.yaml = {
    homeManager =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [
        ];
        programs.emacs.extraPackages =
          epkgs: with epkgs; [
          ];
        editor.initFiles.markdown.text = ''
          (require 'markdown-ts-mode)
        '';
      };
  };
}
# Markdown:1 ends here
