#!/usr/bin/env bash

curl https://nixos.org/nix/install | sh

nix-env -i haskellPackages.stack

stack --no-terminal setup
stack --no-terminal install hscolour
