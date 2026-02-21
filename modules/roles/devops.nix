# [[file:../../modules.org::*DevOps][DevOps:1]]
{
  flake.aspects =
    { aspects, ... }:
    {
      devops = {
        includes = with aspects; [
          azure
          nginx
          kubernetes
        ];
        homeManager = { };
        nixos = { };
      };
    };
}
# DevOps:1 ends here
