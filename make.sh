#!/bin/sh

cabal install -j
runghc test/Main.hs
cp .cabal-sandbox/bin/HikiWiki .

