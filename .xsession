#!/usr/bin/env zsh
# export NO_AT_BRIDGE=1

if [[ "$PATH" != *".local"* ]]; then
  PATH=$HOME/.local/bin:$PATH
fi
if [[ "$PATH" != *"asdf"* ]]; then
  PATH=$HOME/.asdf/shims:$PATH
fi
export PATH

if [ -e $HOME/.fehbg ]; then
  . $HOME/.fehbg
fi

xset -b b off

if type syndaemon >/dev/null; then
  # If syndaemon is installed, we're on a laptop.
  syndaemon -dki 0.5
  setxkbmap -layout us,hu,il -variant ,,phonetic -option ctrl:nocaps -option grp:alt_space_toggle
  xcape -e '#66=Escape'
else
  setxkbmap -layout us,hu,il -variant ,,phonetic -option grp:alt_space_toggle
fi
picom -b
unclutter -b --exclude-root --jitter 5
exec xmonad
