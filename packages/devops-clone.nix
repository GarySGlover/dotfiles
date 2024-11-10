# [[file:packages.org::*nix derivation][nix derivation:1]]
{ pkgs, ... }:
{
  devops-clone = pkgs.writeShellApplication {
    name = "devops-clone";
    runtimeInputs = with pkgs; [
      # azure-cli
      gum
      jq
      curl
    ];
    text = builtins.readFile ./devops-clone;
  };
}
# nix derivation:1 ends here
