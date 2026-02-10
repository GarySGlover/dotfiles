{
  config,
  lib,
  ...
}:
let
in
with lib;
with types;
{
  options.wolf = {
    secretsPath = mkOption { type = path; };
  };
}
