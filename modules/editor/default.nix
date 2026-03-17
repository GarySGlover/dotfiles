# [[file:../../modules.org::*Editor][Editor:1]]
{
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
            { lib, ... }:
            {
              options.editor.earlyInitFiles = lib.mkOption {
                type = lib.types.attrsOf (
                  lib.types.submodule {
                    options = {
                      text = lib.mkOption {
                        type = lib.types.str;
                        description = "Contents of the earlyInit file.";
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
                  Emacs early init code files.
                '';
                default = { };
              };
              options.editor.initFiles = lib.mkOption {
                type = lib.types.attrsOf (
                  lib.types.submodule {
                    options = {
                      text = lib.mkOption {
                        type = lib.types.str;
                        description = "Contents of the earlyInit file.";
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
                  Emacs init code files.
                '';
                default = { };
              };
            };
        };
        editor = {
          includes = with aspects; [ ];
          homeManager =
            { pkgs, config, ... }:
            {
              config = {
                programs.emacs = {
                  enable = true;
                  package = pkgs.emacs-pgtk;
                };
                services.emacs = {
                  enable = true;
                  defaultEditor = true;
                  startWithUserSession = "graphical";
                };

                xdg.configFile = lib.mkMerge (
                  (lib.mapAttrsToList (name: cfg: {
                    "emacs/init-${name}.el" = {
                      text = lib.strings.concatLines [
                        ";; -*- lexical-binding: t -*-"
                        cfg.text
                      ];
                    };
                  }) config.editor.initFiles)
                  ++ (lib.mapAttrsToList (name: cfg: {
                    "emacs/early-init-${name}.el" = {
                      text = lib.strings.concatLines [
                        ";; -*- lexical-binding: t -*-"
                        cfg.text
                      ];
                    };
                  }) config.editor.earlyInitFiles)
                  ++ [
                    {
                      "emacs/init.el".text =
                        let
                          files = lib.attrValues (lib.mapAttrs (name: v: v // { inherit name; }) config.editor.initFiles);
                          cmpP = a: b: (a.priority - b.priority) < 0;
                          sorted = lib.sort cmpP files;
                          mkIncludes =
                            files:
                            lib.concatMapStringsSep "\n" (
                              x: ''(load (expand-file-name "init-${x.name}.el" user-emacs-directory))''
                            ) files;
                        in
                        mkIncludes sorted;
                    }
                    {
                      "emacs/early-init.el".text =
                        let
                          files = lib.attrValues (
                            lib.mapAttrs (name: v: v // { inherit name; }) config.editor.earlyInitFiles
                          );
                          cmpP = a: b: (a.priority - b.priority) < 0;
                          sorted = lib.sort cmpP files;
                          mkIncludes =
                            files:
                            lib.concatMapStringsSep "\n" (
                              x: ''(load (expand-file-name "early-init-${x.name}.el" user-emacs-directory))''
                            ) files;
                        in
                        mkIncludes sorted;
                    }
                  ]
                );
              };
            };
        };
      };
    };
}
# Editor:1 ends here
