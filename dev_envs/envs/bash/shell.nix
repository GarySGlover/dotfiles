{ pkgs }:
let
  myPackages = with pkgs; [
    shfmt
    shellcheck
  ];
in
{
  packages = myPackages;
  shell = pkgs.mkShell {
    name = "bash";
    packages = myPackages;
  };
}
