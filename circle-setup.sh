#!/usr/bin/env bash

export PATH=$HOME/.nix-profile/bin:$PATH
export CURL_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt
export NIX_PATH=$HOME/.nix-defexpr/channels

mkdir -p nix-store
sudo mkdir -p /nix
sudo chown ubuntu /nix
mv nix-store /nix/store

curl https://nixos.org/nix/install | sh

nix-env -i stack

stack --nix --no-terminal setup
stack --nix --no-terminal install hscolour
