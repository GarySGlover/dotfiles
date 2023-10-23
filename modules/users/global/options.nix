{
  lib,
  pkgs,
  ...
}:
with lib; {
  options.wolf = {
    host = mkOption {
      type = types.str;
    };
    secretsPath = mkOption {
      type = types.path;
    };
    user.interactive = mkOption {
      type = types.bool;
    };
  };
}
