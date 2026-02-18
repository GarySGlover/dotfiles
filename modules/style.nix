# [[file:../modules.org::*Style][Style:1]]
{
  inputs,
  ...
}:
{
  flake.aspects = {
    style = {
      homeManager =
        { pkgs, ... }:
        {
          imports = [ inputs.stylix.homeModules.stylix ];
          config = {
            stylix.enable = true;
            stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/one-light.yaml";
          };
        };
    };
  };
}
# Style:1 ends here
