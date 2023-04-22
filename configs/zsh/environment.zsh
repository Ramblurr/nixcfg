#!/usr/bin/env zsh

#█▓▒░ the one, the only zsh
export SHELL=/usr/bin/zsh

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
#█▓▒░ This path our people walked
export PATH=$HOME/vendor/clojure/bin:$HOME/.local/share/go/bin:$HOME/.local/dotbin:$HOME/.local/bin:/usr/local/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH


#█▓▒░ stop barebacking my ~/ goddammit
export XDG_CACHE_HOME=$HOME/.cache
export XDG_DATA_HOME=$HOME/.local/share
export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_DIRS=/home/ramblurr/.local/share:/home/ramblurr/.local/share/flatpak/exports/share:/var/lib/flatpak/exports/share:/usr/local/share:/usr/share
export KUBECONFIG=$XDG_CONFIG_HOME/kube/config
export KUBE_CONFIG_PATH=$KUBECONFIG
export LEIN_HOME=$XDG_DATA_HOME/lein
export M2_HOME=$XDG_DATA_HOME/m2
export PSQL_HISTORY=$XDG_CACHE_HOME/psql_history
export VOLTA_HOME=$XDG_DATA_HOME/volta
export NPM_PACKAGES=$XDG_CACHE_HOME/npm-packages
export NPM_CONFIG_USERCONFIG=$XDG_CONFIG_HOME/npm/npmrc
if [[ -d "$NPM_PACKAGES" ]]; then
  export PATH=$NPM_PACKAGES/bin:$PATH
fi

if [[ -d "$VOLTA_HOME/bin" ]]; then
  export PATH=$XDG_DATA_HOME/npm/bin:$VOLTA_HOME/bin:$PATH
fi

#█▓▒░ all the evil things in the world have full sway
if [ "$XDG_SESSION_DESKTOP" = "sway" ] || [ "$XDG_CURRENT_DESKTOP" = "sway" ]; then
    # https://github.com/swaywm/sway/issues/595
    export _JAVA_AWT_WM_NONREPARENTING=1
    export KITTY_ENABLE_WAYLAND=1 kitty
fi

#█▓▒░ if you're using podman, you're probably not using docker
if command -v podman && ! command -v docker; then
  export DOCKER=podman
  export DOCKER_HOST=unix:///run/user/1000/podman/podman.sock
fi