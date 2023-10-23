#!/usr/bin/env bash
pushd $(dirname -- "$0") > /dev/null && pushd $(git rev-parse --show-toplevel) >> /dev/null || exit 1

nix build .#homeConfigurations.${USER}@${HOSTNAME}.activationPackage --extra-experimental-features nix-command --extra-experimental-features flakes --show-trace && ./result/activate


popd > /dev/null && popd > /dev/null || exit 1
