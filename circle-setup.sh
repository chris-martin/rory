#!/usr/bin/env bash

export PATH=$HOME/.nix-profile/bin:$PATH
export CURL_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt
export NIX_PATH=$HOME/.nix-defexpr/channels

# Create a nix-store dir in the home directory in case it isn't already
# populated by the Circle cache.
mkdir -p nix-store

# Move the cached Nix store to its real location, /nix/store
sudo mkdir -p /nix
sudo chown ubuntu /nix
mv nix-store /nix/store

# Install Nix
curl https://nixos.org/nix/install | sh

# Install Stack
nix-env -i stack

stack --nix --no-terminal setup
stack --nix --no-terminal install hscolour

# Copy the store into the home directory where it can be cached by Circle.
cp -r /nix/store ~/nix-store
