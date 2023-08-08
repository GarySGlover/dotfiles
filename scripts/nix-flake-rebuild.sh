#!/usr/bin/env bash
pushd $(dirname -- "$0") > /dev/null && pushd $(git rev-parse --show-toplevel) >> /dev/null || exit 1
sudo nixos-rebuild switch --flake .#
popd > /dev/null && popd > /dev/null || exit 1
