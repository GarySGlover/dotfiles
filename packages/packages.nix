# [[file:packages.org::*Local Nix packages][Local Nix packages:1]]
{ pkgs, ... }:
{
  inherit (import ./ak9s.nix { inherit pkgs; }) ak9s;
  inherit (import ./devops-clone.nix { inherit pkgs; }) devops-clone;
  inherit (import ./e.nix { inherit pkgs; }) e;
}
# Local Nix packages:1 ends here
