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
    host = mkOption {
      type = str;
    };
    secretsPath = mkOption {
      type = path;
    };
    user.interactive = mkOption {
      type = bool;
    };
  };

  options.wolf.languages = {
    python = mkOption {
      type = bool;
      default = roles.programming;
    };
    nix = mkOption {
      type = bool;
      default = roles.programming;
    };
    json = mkOption {
      type = bool;
      default = roles.programming;
    };
    nim = mkOption {
      type = bool;
      default = roles.programming;
    };
  };

  options.wolf.roles = {
    devops = mkOption {
      type = bool;
      default = false;
    };
    desktop = mkOption {
      type = bool;
      default = roles.gaming;
    };
    gaming = mkOption {
      type = bool;
      default = false;
    };
    gui = mkOption {
      type = bool;
      default = roles.gaming || roles.desktop;
    };
    programming = mkOption {
      type = bool;
      default = false;
    };
    editing = mkOption {
      type = bool;
      default = roles.internet || roles.programming;
    };
    internet = mkOption {
      type = bool;
      default = false;
    };
  };
}
