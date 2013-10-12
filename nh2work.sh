#!/bin/bash

set -e
echo 'Creating nh2work branch'

git checkout nh2work
git reset --hard build-executables-in-parallel
git merge avoid-linking dont-touch-files
# fix-best-version       only for development, maybe already merged
# profiling-in-parallel  unfortunately not correct due to https://github.com/haskell/cabal/pull/1413
