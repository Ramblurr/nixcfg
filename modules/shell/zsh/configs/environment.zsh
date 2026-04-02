#!/usr/bin/env zsh

#█▓▒░ the one, the only zsh
export SHELL=/usr/bin/zsh
if [[ -f /run/current-system/sw/bin/zsh ]]; then
  export SHELL=/run/current-system/sw/bin/zsh
elif [[ -f /usr/bin/zsh ]]; then
  export SHELL=/usr/bin/zsh
fi

#█▓▒░ preferred editor for local and remote sessions
export EDITOR=vim
export VISUAL=vim

#█▓▒░ language
export LC_COLLATE=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export LC_MESSAGES=en_US.UTF-8
export LC_MONETARY=en_US.UTF-8
export LC_NUMERIC=en_US.UTF-8
export LC_TIME=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export LESSCHARSET=utf-8

#█▓▒░ kde file picker > gnome file picker
export GTK_USE_PORTAL=1

#█▓▒░ emacs lsp-mode vroom
export LSP_USE_PLISTS=true

#█▓▒░ stop barebacking my ~/ goddammit
export XDG_CACHE_HOME=$HOME/.cache
export XDG_DATA_HOME=$HOME/.local/share
export XDG_CONFIG_HOME=$HOME/.config
export XDG_STATE_HOME=$HOME/.local/state

#if [[ -d "$VOLTA_HOME/bin" ]]; then
#  export PATH=$XDG_DATA_HOME/npm/bin:$VOLTA_HOME/bin:$PATH
#fi

#█▓▒░ if you're using podman, you're probably not using docker
if command -v podman &>/dev/null && ! command -v docker &>/dev/null; then
  export DOCKER=podman
  export DOCKER_HOST=unix:///run/user/1000/podman/podman.sock
fi

#█▓▒░ fzf should ignore things
#export FZF_DEFAULT_COMMAND='fd --type f --strip-cwd-prefix'
#█▓▒░ direnv is a thing
if command -v direnv &>/dev/null; then
  eval "$(direnv hook zsh)"
fi
