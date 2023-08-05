#!/usr/bin/env bash
pushd $(dirname -- "$0") > /dev/null || exit 1
sudo nixos-rebuild switch --flake $(git rev-parse --show-toplevel)/.#
popd > /dev/null || exit 1
