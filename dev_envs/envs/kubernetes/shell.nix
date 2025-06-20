{ pkgs }:
let
  myPackages = with pkgs; [
    kubectl
    kubelogin
    kubernetes-helm
  ];
in
{
  packages = myPackages;
  shell = pkgs.mkShell {
    name = "kubernetes";
    packages = myPackages;
  };
}
