#!/bin/sh
runhaskell ParserMain.hs $1 | diff -w $1 -
