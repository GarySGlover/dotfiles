{ pkgs }:
let
  myPackages = with pkgs; [
    podman
    podman-tui
    podman-compose
  ];
in
{
  packages = myPackages;
  shell = pkgs.mkShell {
    name = "podman";
    packages = myPackages;
  };
}
