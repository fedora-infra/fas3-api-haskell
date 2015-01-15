#!/usr/bin/env bash

cwd="$( cd "${BASH_SOURCE[0]%/*}" && pwd )"
cd "$cwd/.."
f=`mktemp -d`
git clone "git@github.com:fedora-infra/fas3-api-haskell.git" "$f/fas3-api-haskell.git"
cabal haddock
pushd "$f/fas3-api-haskell.git"
  git checkout gh-pages && git rm -rf *
popd
mv dist/doc/html/fas3/* "$f/fas3-api-haskell.git/"
pushd "$f/fas3-api-haskell.git"
  git add -A
  git commit -m "Deploy haddock docs to github-pages."
  git push origin gh-pages
popd
rm -rf "$f"

if [ $? == 0 ]; then
  echo "*** Done: https://fedora-infra.github.io/fas3-api-haskell/"
  exit 0
else
  echo "*** ERROR!!! Fix the above and try again."
  exit 1
fi
