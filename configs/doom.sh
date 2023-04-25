#!/usr/bin/env sh

DOOM="$HOME/.emacs.d"

if [ ! -d "$DOOM" ]; then
  git clone https://github.com/hlissner/doom-emacs.git $DOOM
  kitty $DOOM/bin/doom -y install & disown
else
  kitty $DOOM/bin/doom sync
fi
