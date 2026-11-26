# [[file:../modules.org::*YAML][YAML:1]]
{
  flake.aspects.yaml = {
    homeManager =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [
          yq
          yaml-language-server
        ];
      };
  };
}
# YAML:1 ends here
