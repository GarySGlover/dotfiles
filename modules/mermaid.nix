# [[file:../modules.org::*Mermaid][Mermaid:1]]
{
  flake.aspects.mermaid = {
    homeManager =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [
          mermaid-cli
        ];
      };
  };
}
# Mermaid:1 ends here
