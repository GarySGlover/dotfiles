{ pkgs }:
let
  myPackages = with pkgs; [
    yamlfmt
    yamllint
  ];
in
{
  packages = myPackages;
  shell = pkgs.mkShell {
    name = "yaml";
    packages = myPackages;
  };
}
