# [[file:packages.org::*nix derivation][nix derivation:1]]
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
# nix derivation:1 ends here
