#!/bin/sh

./clean.sh
ghc --make HikiWiki.hs
ghc --make post-update.hs

