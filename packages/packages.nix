# [[file:packages.org::*Local Nix packages][Local Nix packages:1]]
{ pkgs, ... }:
{
  inherit (import ./ak9s.nix { inherit pkgs; }) ak9s;
}
# Local Nix packages:1 ends here
