#!/usr/bin/env bash
pushd "$(dirname -- "$0")" > /dev/null && pushd "$(git rev-parse --show-toplevel)" >> /dev/null || exit 1
nix repl --extra-experimental-features 'flakes repl-flake nix-command' ".#$1"
popd > /dev/null && popd > /dev/null || exit 1
