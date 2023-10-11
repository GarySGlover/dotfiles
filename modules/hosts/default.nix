{
  lib,
  config,
  ...
}: let
  subModules = lib.lists.forEach (builtins.attrNames (
    lib.filterAttrs (n: v: v == "directory")
    (builtins.readDir ./.)
  )) (x: ./. + "/${x}");
  nixFiles = lib.lists.forEach (builtins.attrNames (
    lib.filterAttrs (n: v: lib.strings.hasSuffix ".nix" n && n != "default.nix" && v == "regular")
    (builtins.readDir ./.)
  )) (x: ./. + "/${x}");
in {
  imports = subModules ++ nixFiles;
}
