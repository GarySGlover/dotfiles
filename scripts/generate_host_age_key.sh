#!/usr/bin/env bash
mkdir ~/.ssh
ssh-keygen -t ed25519 -f ~/.ssh/"$1"_ed25519 -N ""
ssh-keygen -f ~/.ssh/"$1"_ed25519 -y | ssh-to-age >~/.config/sops/age/keys.txt
