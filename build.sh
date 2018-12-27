#!/usr/bin/env  bash

cabal2nix . > default.nix
cabal2nix --shell . > shell.nix
nix-shell --command 'cabal configure --enable-tests'
cabal build
cabal test
