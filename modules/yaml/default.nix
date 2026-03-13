# [[file:../../modules.org::*YAML][YAML:1]]
{
  flake.aspects.yaml = {
    homeManager = { pkgs, ... }:
    {
      home.packages = with pkgs; [
        yq
      ];
    };
  };
}
# YAML:1 ends here
