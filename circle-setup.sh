#!/usr/bin/env bash

pushd $(mktemp -d)
wget https://github.com/commercialhaskell/stack/releases/download/v1.0.4/stack-1.0.4.2-linux-x86_64.tar.gz
tar -xzf stack-1.0.4.2-linux-x86_64.tar.gz
sudo mv stack-1.0.4.2-linux-x86_64/stack /usr/bin/
popd

sudo apt-get install -y libgmp-dev libsystemd-journal-dev

stack --no-terminal setup
stack --no-terminal install hscolour
