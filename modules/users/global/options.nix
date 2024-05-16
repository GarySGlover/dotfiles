{
  lib,
  pkgs,
  config,
  ...
}:
with lib;
with types; let
  roles = config.wolf.roles;
in {
  options.wolf = {
    host = mkOption {type = str;};
    secretsPath = mkOption {type = path;};
    user.interactive = mkOption {type = bool;};
  };

  options.wolf.languages = {
    go = mkOption {type = bool;};
    json = mkOption {type = bool;};
    lisp = mkOption {type = bool;};
    nim = mkOption {type = bool;};
    nix = mkOption {type = bool;};
    powershell = mkOption {type = bool;};
    python = mkOption {type = bool;};
    zig = mkOption {type = bool;};
  };

  options.wolf.roles = {
    cad = mkOption {type = bool;};
    desktop = mkOption {type = bool;};
    devops = mkOption {type = bool;};
    editing = mkOption {type = bool;};
    gaming = mkOption {type = bool;};
    internet = mkOption {type = bool;};
    programming = mkOption {type = bool;};
    work = mkOption {type = bool;};
  };
}
