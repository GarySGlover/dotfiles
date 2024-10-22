{ pkgs, lib, ... }:
let
  myPackages = with pkgs; [
    pwvucontrol
    pavucontrol
    helvum
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
