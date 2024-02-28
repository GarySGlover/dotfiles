#!/usr/bin/env bash
pushd $(dirname -- "$0") > /dev/null && pushd $(git rev-parse --show-toplevel) >> /dev/null || exit 1

./scripts/nix-trim-generations.sh 1 0 home-manager
nix-collect-garbage
nix-store --optimise --verbose
