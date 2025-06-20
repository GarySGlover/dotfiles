{ pkgs }:
let
  myPackages = with pkgs; [
    pre-commit
  ];
in
{
  packages = myPackages;
  shell = pkgs.mkShell {
    name = "pre-commit";
    packages = myPackages;
  };
}
