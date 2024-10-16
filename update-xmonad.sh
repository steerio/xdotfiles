#!/usr/bin/env zsh

cd
cabal update

link () {
  local bin=$(jq -r '.["install-plan"][] | select(.["component-name"] == "exe:'$1'")["bin-file"]' dist-newstyle/cache/plan.json)
  if [[ -f $bin ]]; then
    local bname=$(basename $bin)
    local dest=~/.local/bin/$bname
    if [[ ! -f $dest || $(stat -Lc %d:%i $bin) != $(stat -Lc %d:%i $dest) ]]; then
      echo "=== Linking $bname"
      strip $bin
      ln -f $(realpath --relative-to=. $bin) $dest
    fi
  fi
}

for i in ~/.{xmonad,xmobar}/*-git; do
  echo "=== Pulling $(basename $i)"
  cd $i
  git pull
done

cd ~/.xmonad
echo "=== Building xmonad"
cabal build xmonad --upgrade-dependencies
link xmonad

echo "=== Building xmobar"
cd ~/.xmobar
cabal build xmobar --upgrade-dependencies
link xmobar
