#!/bin/sh
./parse.sh $1 | diff -w $1 -
