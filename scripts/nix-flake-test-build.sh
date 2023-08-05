#!/usr/bin/env bash
pushd $(dirname -- "$0") > /dev/null || exit 1
pushd $(git rev-parse --show-toplevel) > /dev/null || exit 1
nixos-rebuild build --flake $(git rev-parse --show-toplevel)/.#
popd > /dev/null || exit 1
popd > /dev/null || exit 1
