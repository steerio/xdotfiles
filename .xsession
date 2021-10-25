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

setxkbmap -layout us,hu,il -variant ,,phonetic -option grp:alt_space_toggle
picom -b
unclutter -b --exclude-root --jitter 5
exec xmonad
