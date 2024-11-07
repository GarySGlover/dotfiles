# [[file:packages.org::*Derivation][Derivation:1]]
{ pkgs, ... }:
{
  ak9s = pkgs.writeShellApplication {
    name = "ak9s";
    runtimeInputs = with pkgs; [
      k9s
      azure-cli
      gum
      jq
      kubectl
    ];
    text = builtins.readFile ./ak9s;
  };
}
# Derivation:1 ends here
