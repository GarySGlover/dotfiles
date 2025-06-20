{ pkgs }:
let
  myPackages = with pkgs; [
    docker
  ];
in
{
  packages = myPackages;
  shell = pkgs.mkShell {
    name = "docker";
    packages = myPackages;
  };
}
