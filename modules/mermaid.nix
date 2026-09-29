# [[file:../modules.org::*Mermaid][Mermaid:1]]
{
  flake.aspects.mermaid = {
    homeManager =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [
          mermaid-cli
        ];
        programs.emacs.extraPackages =
          epkgs: with epkgs; [
            mermaid-mode
            ob-mermaid
          ];
        editor.initFiles.mermaid.text = ''

        '';
      };
  };
}
# Mermaid:1 ends here
