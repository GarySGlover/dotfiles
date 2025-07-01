{ lib, ... }:
{
  removeHashPrefix =
    str:
    if builtins.substring 0 1 str == "#" then
      builtins.substring 1 (builtins.stringLength str - 1) str
    else
      str;
  capitalize =
    s:
    let
      len = builtins.stringLength s;
    in
    lib.strings.toUpper (lib.strings.substring 0 1 s) + lib.strings.substring 1 (len - 1) s;
}
