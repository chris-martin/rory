#!/usr/bin/env bash

export PATH=$HOME/.nix-profile/bin:$PATH
export CURL_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt
export NIX_PATH=$HOME/.nix-defexpr/channels

stack --nix --no-terminal build
