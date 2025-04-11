#!/usr/bin/env sh

DOOM="$XDG_CONFIG_HOME/emacs"

if [ ! -d "$DOOM" ]; then
    echo "Doom emacs is not installed at $DOOM"
    exit 1
else
  kitty $DOOM/bin/doom sync
fi
