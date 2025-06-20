{ pkgs }:
let
  myPackages = with pkgs; [
    azure-cli
    kubelogin
  ];
in
{
  packages = myPackages;
  shell = pkgs.mkShell {
    name = "azure";
    packages = myPackages;
  };
}
