# [[file:packages.org::*nix derivation][nix derivation:1]]
{ pkgs, ... }:
{
  kc = pkgs.writeShellApplication {
    name = "kc";
    runtimeInputs = with pkgs; [
      k9s
      azure-cli
      gum
      jq
      kubectl
      kubelogin
    ];
    text = builtins.readFile ./kc;
  };
}
# nix derivation:1 ends here
