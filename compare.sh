#!/bin/sh
./parse.sh $1 | diff $1 -
