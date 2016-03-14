#!/usr/bin/env bash

curl https://nixos.org/nix/install | sh

export PATH=$HOME/.nix-profile/bin:$PATH

nix-env -i haskellPackages.stack

stack --no-terminal setup
stack --no-terminal install hscolour
