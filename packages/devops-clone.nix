# [[file:packages.org::*nix derivation][nix derivation:1]]
{ pkgs, ... }:
let
  local-pkgs = (import ./packages.nix { inherit pkgs; });
in
{
  devops-clone = pkgs.writeShellApplication {
    name = "devops-clone";
    runtimeInputs = with pkgs; [
      # azure-cli
      gum
      jq
      curl
      local-pkgs.azure-tools
    ];
    text = builtins.readFile ./devops-clone;
  };
}
# nix derivation:1 ends here
