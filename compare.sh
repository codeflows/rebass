#!/bin/sh
runhaskell ParserMain.hs $1 | diff $1 -