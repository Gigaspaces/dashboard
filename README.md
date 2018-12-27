# Installations

### haskell-platform
sudo apt-get install haskell-platform

### haskell-stack
sudo apt install haskell-stack

##### Add dependency in the file `package.yaml`
##### Build `stack build`
##### Run `stack run`
##### Use REPL `stack repl`





### with Nixos and cable

cabal2nix . > default.nix
cabal2nix --shell . > shell.nix
nix-shell --command 'cabal configure'
nix-shell --command 'cabal configure --enable-tests'
cabal build

nix-shell -- for ghc
