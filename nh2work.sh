#!/bin/bash

set -e
echo 'Creating nh2work branch'

git checkout nh2work
git reset --hard build-executables-in-parallel
git merge avoid-linking profiling-in-parallel flush-stdout dont-touch-files fix-best-version
