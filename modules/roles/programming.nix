# [[file:../../modules.org::*Programming][Programming:1]]
{
  flake.aspects =
    { aspects, ... }:
    {
      programming = {
        includes = with aspects; [
          direnv
          go
          guix
          mermaid
          yaml
        ];
        homeManager = {
          fonts.fontconfig.enable = true;
        };
        nixos = { };
      };
    };
}
# Programming:1 ends here
