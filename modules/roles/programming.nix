# [[file:../../modules.org::*Programming][Programming:1]]
{
  flake.aspects =
    { aspects, ... }:
    {
      programming = {
        includes = with aspects; [
          direnv
          docker
          go
          yaml
        ];
        homeManager = { };
        nixos = { };
      };
    };
}
# Programming:1 ends here
