#!/usr/bin/zsh

setopt extended_glob
cd `dirname $0`

src=${$(pwd)#$HOME/}
cfg=.config

mkdir -p ~/$cfg

for i in (.*~.git*~$cfg); do
  echo "Linking $i"
  ln -s $src/$i $HOME/
done

for i in .config/*; do
  echo "Linking $i"
  ln -s ../$src/$i ~/$cfg/
done
