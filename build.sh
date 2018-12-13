#!/usr/bin/env  bash 

cabal2nix . > default.nix
cabal2nix --shell . > shell.nix
nix-shell --command 'cabal configure'
cabal build
