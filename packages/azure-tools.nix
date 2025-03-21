# [[file:packages.org::*nix derivation][nix derivation:1]]
{ pkgs, ... }:
let
  azure-token = pkgs.writeShellApplication {
    name = "azure-token";
    runtimeInputs = with pkgs; [ skate ];
    text = builtins.readFile ./azure-token;
  };
  azure-get = pkgs.writeShellApplication {
    name = "azure-get";
    runtimeInputs = with pkgs; [ curl ];
    text = builtins.readFile ./azure-get;
  };
in
{
  azure-tools = pkgs.stdenv.mkDerivation {
    name = "azure-tools";
    buildInputs = [
      azure-get
    ];
    buildCommand = ''
      mkdir -p $out/bin
      ln -s ${azure-token}/bin/azure-token $out/bin/
      ln -s ${azure-get}/bin/azure-get $out/bin/
    '';
  };
}
# nix derivation:1 ends here
