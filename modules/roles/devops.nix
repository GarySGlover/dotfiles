# [[file:../../modules.org::*DevOps][DevOps:1]]
{
  flake.aspects =
    { aspects, ... }:
    {
      devops = {
        includes = with aspects; [
          azure
          docker
          kubernetes
          terraform
          nginx
        ];
        homeManager = { };
        nixos = { };
      };
    };
}
# DevOps:1 ends here
