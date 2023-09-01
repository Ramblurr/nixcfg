#!/usr/bin/env zsh

source $ZDOTDIR/autocompletion.zsh .zsh
source $ZDOTDIR/environment.zsh
source $ZDOTDIR/keybindings.zsh
source $ZDOTDIR/privacy.zsh
#source $ZDOTDIR/gpg-ssh.zsh

if command -v starship &>/dev/null; then
  if [[ $TERM != "dumb" && (-z $INSIDE_EMACS || $INSIDE_EMACS == "vterm") ]]; then
    eval "$(starship init zsh)"
  fi
else
  source $ZDOTDIR/prompt.zsh
fi
