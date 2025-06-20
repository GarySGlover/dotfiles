{ pkgs }:
let
  myPackages = with pkgs; [
    nixfmt-rfc-style
    nixd
  ];
in
{
  packages = myPackages;
  shell = pkgs.mkShell {
    name = "nix";
    packages = myPackages;
  };
}
