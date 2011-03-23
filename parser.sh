#!/bin/sh
echo "Input parsed and then serialized:"
runhaskell ParserMain.hs $1
echo
echo "Differences between original and rebassed:"
runhaskell ParserMain.hs $1 | diff -w $1 -
