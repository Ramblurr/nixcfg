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
export KUBECONFIG=$XDG_CONFIG_HOME/kube/config
export KREW_ROOT=$XDG_CONFIG_HOME/krew
export KUBE_CONFIG_PATH=$KUBECONFIG
export LEIN_HOME=$XDG_DATA_HOME/lein
export PSQL_HISTORY=$XDG_CACHE_HOME/psql_history
export VOLTA_HOME=$XDG_DATA_HOME/volta
export NPM_PACKAGES=$XDG_CACHE_HOME/npm-packages
export NPM_CONFIG_USERCONFIG=$XDG_CONFIG_HOME/npm/npmrc
export CLJ_CONFIG=$XDG_CONFIG_HOME/clojure
export CLJ_CACHE=$XDG_CACHE_HOME/clojure
export DEPS_CLJ_TOOLS_DIR=$XDG_DATA_HOME/deps.clj
export GITLIBS=$XDG_CACHE_HOME/clojure-gitlibs
export DOCKER_CONFIG=$XDG_CONFIG_HOME/docker
export CLAUDE_CONFIG_DIR=$XDG_CONFIG_HOME/claude
export CODEX_HOME=$XDG_CONFIG_HOME/codex

#█▓▒░ This path our people walked

export PATH=$HOME/vendor/clojure/bin:$HOME/.local/share/go/bin:$HOME/.local/dotbin:$HOME/.local/bin:/usr/local/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH

if [[ -d "$NPM_PACKAGES" ]]; then
  export PATH=$NPM_PACKAGES/bin:$PATH
fi

#if [[ -d "$VOLTA_HOME/bin" ]]; then
#  export PATH=$XDG_DATA_HOME/npm/bin:$VOLTA_HOME/bin:$PATH
#fi

if [[ -d "$KRE_ROOT/bin" ]]; then
  export PATH=$KREW_ROOT/bin:$PATH
fi

#█▓▒░ all the evil things in the world have full sway
if [ "$XDG_SESSION_DESKTOP" = "sway" ] || [ "$XDG_CURRENT_DESKTOP" = "sway" ] || [ "$XDG_SESSION_TYPE" = "wayland" ]; then
  # https://github.com/swaywm/sway/issues/595
  export _JAVA_AWT_WM_NONREPARENTING=1
  export KITTY_ENABLE_WAYLAND=1 kitty
fi

#█▓▒░ if you're using podman, you're probably not using docker
if command -v podman &>/dev/null && ! command -v docker &>/dev/null; then
  export DOCKER=podman
  export DOCKER_HOST=unix:///run/user/1000/podman/podman.sock
fi

#█▓▒░ fzf should ignore things
export FZF_DEFAULT_COMMAND='fd --type f --strip-cwd-prefix'
#█▓▒░ direnv is a thing
if command -v direnv &>/dev/null; then
  eval "$(direnv hook zsh)"
fi
