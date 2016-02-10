#!/bin/sh

#cabal update
cabal sandbox init
cabal install -j
cabal test
cp .cabal-sandbox/bin/HikiWiki .

