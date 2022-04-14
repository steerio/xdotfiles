#!/usr/bin/env zsh

cd
cabal update

for i in ~/.{xmonad,xmobar}/*-git; do
  echo "=== Pulling $(basename $i)"
  cd $i
  git pull
done

cd ~/.xmonad
echo "=== Building xmonad"
cabal build xmonad xmonad-contrib

echo "=== Building xmobar"
cd ~/.xmobar
cabal build xmobar
