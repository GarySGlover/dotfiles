{ pkgs, ... }:
{
  home.packages = with pkgs; [
    (azure-cli.override {
      withImmutableConfig = false;
      withExtensions = with pkgs.azure-cli-extensions; [
        interactive
        azure-devops
      ];
    })
    kubectl
    kubelogin
  ];
}
