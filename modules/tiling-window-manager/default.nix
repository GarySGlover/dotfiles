# [[file:../../modules.org::*Tiling Window Manager][Tiling Window Manager:1]]
{
  ...
}:
{
  flake.aspects =
    { aspects, ... }:
    {
      config = {
        options = {
          homeManager =
            { lib, ... }:
            {
              options.niri.defaultConfig = lib.mkOption {
                type = lib.types.str;
                default = "";
              };
              options.niri.configFiles = lib.mkOption {
                type = lib.types.attrsOf (
                  lib.types.submodule {
                    options = {
                      text = lib.mkOption {
                        type = lib.types.str;
                        description = "Contents of the config file.";
                      };
                      priority = lib.mkOption {
                        type = lib.types.int;
                        default = 1000;
                        description = "Priority (default: 1000).";
                      };
                    };
                  }
                );
                description = ''
                  Config files for niri, keyed by arbitrary name.
                  Duplicate keys are forbidden.
                '';
              };
            };
        };
        tiling-window-manager = {
          includes = with aspects; [ gui ];
          homeManager =
            { lib, config, ... }:
            {
              config = {
                services.mako.enable = true;
                xdg.configFile = lib.mkMerge (
                  (lib.mapAttrsToList (name: cfg: {
                    "niri/${name}.kdl" = {
                      text = cfg.text;
                    };
                  }) config.niri.configFiles)
                  ++ [
                    {
                      "niri/config.kdl".text =
                        let
                          splitConfigFiles =
                            attrs:
                            let
                              files = lib.attrValues (lib.mapAttrs (name: v: v // { inherit name; }) attrs);
                              lower = builtins.filter (x: x.priority < 1000) files;
                              higher = builtins.filter (x: x.priority >= 1000) files;
                              cmpP = a: b: (a.priority - b.priority) > 0;
                            in
                            {
                              lowerSorted = lib.sort cmpP lower;
                              higherSorted = lib.sort cmpP higher;
                            };
                          groups = splitConfigFiles config.niri.configFiles;
                          mkIncludes = files: lib.concatMapStringsSep "\n" (x: ''include "${x.name}.kdl"'') files;
                        in
                        lib.concatStringsSep "\n" [
                          (mkIncludes groups.lowerSorted)
                          config.niri.defaultConfig
                          (mkIncludes groups.higherSorted)
                        ];
                    }
                  ]
                );
              };
            };

          nixos = {
            programs.niri.enable = true;
            environment.systemPackages = [ xwayland-satellite ];
          };
        };
      };
    };
}
# Tiling Window Manager:1 ends here
