#!/usr/bin/env bash
pushd "$(dirname -- "$0")" >/dev/null && pushd "$(git rev-parse --show-toplevel)" >>/dev/null || exit 1

nix flake check ./ --show-trace --all-systems

popd >/dev/null && popd >/dev/null || exit 1
