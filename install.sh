#!/bin/bash
if cabal build; then
  mkdir -p ~/.cabal
  mkdir -p ~/.cabal/bin
  cp dist/build/tsv2csv/tsv2csv ~/.cabal/bin
else
  echo 'The build failed.'
fi
