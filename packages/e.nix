# [[file:packages.org::*nix derivation][nix derivation:1]]
{ pkgs, ... }:
let
  local-pkgs = (import ./packages.nix { inherit pkgs; });
in
{
  e = pkgs.writeShellApplication {
    name = "e";
    runtimeInputs = with pkgs; [
      gum
      local-pkgs.azure-tools
    ];
    text = builtins.readFile ./e;
  };
}
# nix derivation:1 ends here
