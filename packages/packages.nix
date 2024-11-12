# [[file:packages.org::*Local Nix packages][Local Nix packages:1]]
{ pkgs, ... }:
{
  inherit (import ./ak9s.nix { inherit pkgs; }) ak9s;
  inherit (import ./devops-clone.nix { inherit pkgs; }) devops-clone;
  inherit (import ./e.nix { inherit pkgs; }) e;
  inherit (import ./azure-tools.nix { inherit pkgs; }) azure-tools;
}
# Local Nix packages:1 ends here
