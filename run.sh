#!sh

if [ "$#" -lt 1 ]
then
    echo ""
    echo ""
    echo "  usage: run.sh <filename>"
    echo ""
    echo ""
    echo "  example: run.sh input.txt"
    echo ""
    echo ""
    exit 1
fi

echo "Testing $1"

cd haskell
echo ""
echo "------- Running Haskell -------"
cabal run < ../$1
echo "------- End of  Haskell -------"
echo ""

cd ..
cd rust
echo ""
echo "------- Running Rust ----------"
cargo run --release  ../$1
echo "------- End of Rust -----------"
cd ..
