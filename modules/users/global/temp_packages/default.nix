{ pkgs, lib, ... }:
let
  myPackages = with pkgs; [
    # unzip
    # jdk21
    # zip
    # glxinfo
    # wlr-randr
    nwg-displays
    nixfmt-rfc-style
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
