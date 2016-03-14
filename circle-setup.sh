#!/usr/bin/env bash

export PATH=$HOME/.nix-profile/bin:$PATH
export CURL_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt
export NIX_PATH=$HOME/.nix-defexpr/channels
export NIX_STORE_DIR=$HOME/nix-store

mkdir -p ~/nix-store

curl https://nixos.org/nix/install | sh

nix-env -i haskellPackages.stack

stack --nix --no-terminal setup
stack --nix --no-terminal install hscolour
