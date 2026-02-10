#!/usr/bin/env bash
nix-store --verify
pushd "$(dirname -- "$0")" >/dev/null && pushd $(git rev-parse --show-toplevel) >>/dev/null || exit 1
# sudo nixos-rebuild switch --flake .#$1
sudo nixos-rebuild test --flake .#"$1" --show-trace
popd >/dev/null && popd >/dev/null || exit 1
