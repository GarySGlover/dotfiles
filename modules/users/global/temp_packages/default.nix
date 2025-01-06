{ pkgs, lib, ... }:
let
  myPackages = with pkgs; [
  ];
in
{
  config = {
    home.packages = myPackages;
    warnings =
      if myPackages != [ ] then
        [
          "Still using temporary packages: "
        ]
        ++ (map (pkg: pkg.name) myPackages)
      else
        [ ];
  };
}
