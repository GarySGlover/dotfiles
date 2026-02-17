# [[file:../modules.org::*Tiling Window Manager][Tiling Window Manager:1]]
{
  inputs,
  lib,
  ...
}:
{
  flake.aspects =
    { aspects, ... }:
    {
      config = {
        options = {
          homeManager =
            { config, lib, ... }:
            {
              options.niri.configfile = lib.mkOption {
                type = lib.types.string;
                default = "";
              };
            };
        };
        tiling-window-manager = {
          includes = with aspects; [ gui ];
          homeManager.config = {
            services.mako.enable = true;
          };

          nixos = {
            programs.niri.enable = true;
          };
        };
      };
    };
}
# Tiling Window Manager:1 ends here
