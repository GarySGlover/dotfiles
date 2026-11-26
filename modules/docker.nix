# [[file:../modules.org::*Docker][Docker:1]]
{
  flake.aspects.docker.homeManager = { pkgs, ... }: {
    home.packages = with pkgs; [
      docker-language-server
    ];
  };
}
# Docker:1 ends here
