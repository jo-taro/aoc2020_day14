#!sh
cd haskell
cabal build
cd ..
cd rust
cargo build --release
cd ..
