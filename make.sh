#!/bin/sh

cabal test
cabal install -j
cp .cabal-sandbox/bin/HikiWiki .

