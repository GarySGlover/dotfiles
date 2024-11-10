# [[file:packages.org::*nix derivation][nix derivation:1]]
{ pkgs, ... }:
{
  e = pkgs.writeShellApplication {
    name = "e";
    runtimeInputs = with pkgs; [
      gum
    ];
    text = builtins.readFile ./e;
  };
}
# nix derivation:1 ends here
