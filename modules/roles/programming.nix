# [[file:../../modules.org::*Programming][Programming:1]]
{
  flake.aspects =
    { aspects, ... }:
    {
      programming = {
        includes = with aspects; [
          go
        ];
        homeManager = { };
        nixos = { };
      };
    };
}
# Programming:1 ends here
