#!/usr/bin/env bash

nix repl --extra-experimental-features 'flakes repl-flake nix-command' nixpkgs
