#!/usr/bin/env bash
pushd $(dirname -- "$0") > /dev/null && pushd $(git rev-parse --show-toplevel) >> /dev/null || exit 1

nix-store --verify
./scripts/nix-trim-generations.sh 1 0 home-manager
nix-collect-garbage -d
sudo nix-collect-garbage -d
nix-store --optimise --verbose
