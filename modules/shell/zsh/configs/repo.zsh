#
#                 ██
#                ░██
#  ██████  ██████░██
# ░░░░██  ██░░░░ ░██████
#    ██  ░░█████ ░██░░░██
#   ██    ░░░░░██░██  ░██
#  ██████ ██████ ░██  ░██
# ░░░░░░ ░░░░░░  ░░   ░░
#
#  ▓▓▓▓▓▓▓▓▓▓▓
# ░▓ about   ▓ fuzzy select git repo under ~/src
# ░▓ author  ▓ casey <casey@outskirtslabs.com>
# ░▓ code    ▓ https://code.casey.link
# ░▓ license ▓ EUPL-1.2
# ░▓▓▓▓▓▓▓▓▓▓▓
# ░░░░░░░░░░░
#
#█▓▒░
# repo.zsh
# Fuzzy-select a git repo root under ~/src (gopath-like tree) with readable labels.
# Features:
# - Only shows repo roots (directories that contain .git)
# - zoxide "frecent" sort (default if zoxide present), or alphabetical
# - zoxide-like vertically stacked layout (preview pane at bottom)
#
# Usage:
#   repo                # pick repo, cd into it
#   repo -p             # print selected full path
#   repo -o             # open selected repo in $EDITOR
#   repo -A             # alphabetical sort
#   repo -F             # frecent sort (zoxide)
#   repo -t             # toggle sort for this invocation (alpha <-> frecent)
#   repo <query...>     # prefill fzf query
#   repo-link [query...]        # pick repo, symlink it into $PWD
#   repo-link -d DIR [query...]  # pick repo, symlink it into DIR
#
# Env:
#   REPO_BASE=~/src     # change search base
#   REPO_SORT=auto      # auto|frecent|alpha (auto => frecent if zoxide exists)
#
# Recommended:
#   source /path/to/repo.zsh
#   # optional: alias repo=repo  (function name already repo)

repo() {
  setopt local_options pipefail

  local base="${REPO_BASE:-$HOME/src}"
  local mode="cd"
  local TAB=$'\t'
  local sort_mode="${REPO_SORT:-auto}"   # auto|frecent|alpha
  local -a q
  local OPTIND opt

  while getopts ":poAFt" opt; do
    case "$opt" in
      p) mode="print" ;;
      o) mode="open" ;;
      A) sort_mode="alpha" ;;
      F) sort_mode="frecent" ;;
      t)
        if [[ "$sort_mode" == "alpha" ]]; then sort_mode="frecent"; else sort_mode="alpha"; fi
        ;;
    esac
  done
  shift $((OPTIND-1))
  q=("$@")

  [[ -d "$base" ]] || { print -u2 "repo: base not found: $base"; return 1; }

  local has_zoxide=0
  command -v zoxide >/dev/null 2>&1 && has_zoxide=1
  if [[ "$sort_mode" == "auto" ]]; then
    [[ $has_zoxide -eq 1 ]] && sort_mode="frecent" || sort_mode="alpha"
  fi

  # 1) Find repo roots (parents of .git)
  local all_repos
  all_repos="$(
    if command -v bfs >/dev/null 2>&1; then
      bfs -L "$base" -maxdepth 4 -name .git -type d \
        -exclude -name node_modules \
        -exclude -name vendor \
        -exclude -name dist \
        -exclude -name build \
        -exclude -name target \
        -exclude -name .cache \
        -exclude -name .direnv \
        | sed -E 's|/\.git/?$||'
    elif command -v fd >/dev/null 2>&1; then
      fd -H -t d '^\.git$' "$base" \
        --max-depth 4 \
        --exclude node_modules \
        --exclude vendor \
        --exclude dist \
        --exclude build \
        --exclude target \
        --exclude .cache \
        --exclude .direnv \
        | sed -E 's|/\.git/?$||'
    else
      find -L "$base" -type d -name .git -maxdepth 4 -print \
        | sed -E 's|/\.git/?$||'
    fi
  )" || return 1

  [[ -n "$all_repos" ]] || { print -u2 "repo: no repos found under: $base"; return 1; }

  # 2) Load zoxide score list (optional) for display
  local zscores=""
  if [[ $has_zoxide -eq 1 ]]; then
    # lines: "<score> <path>"
    zscores="$(zoxide query -ls 2>/dev/null)"
  fi

  # 3) Label rows:
  #    fullpath \t label(4 segments) \t score
  local labeled_all
  labeled_all="$(
    print -r -- "$all_repos" \
    | awk -v b="$base/" -v zs="$zscores" '
        BEGIN{
          n=split(zs,lines,"\n")
          for(i=1;i<=n;i++){
            if(lines[i]=="") continue
            score=lines[i]; sub(/ .*/,"",score)
            path=lines[i];  sub(/^[^ ]+ /,"",path)
            smap[path]=score
          }
        }
        index($0,b)==1 {
          rel=substr($0,length(b)+1)
          n=split(rel,a,"/")
          if (n>=4) {
            score = ($0 in smap) ? smap[$0] : ""
            label = a[1] "/" a[2] "/" a[3] "/" a[4]
            printf "%s\t%s\t%s\n", $0, label, score
          } else {
            score = ($0 in smap) ? smap[$0] : ""
            printf "%s\t%s\t%s\n", $0, rel, score
          }
        }
      '
  )" || return 1

  # 4) Order rows
  local ordered
  if [[ "$sort_mode" == "frecent" && $has_zoxide -eq 1 ]]; then
    local zlist
    zlist="$(zoxide query -l 2>/dev/null | awk -v b="$base/" 'index($0,b)==1 {print}')" || zlist=""

    ordered="$(
      {
        if [[ -n "$zlist" ]]; then
          print -r -- "$zlist" \
          | while IFS= read -r p; do
              awk -v want="$p" -F "$TAB" '$1==want {print; exit}' <<< "$labeled_all"
            done
        fi
        print -r -- "$labeled_all" \
        | awk -v z="$zlist" -F "$TAB" 'BEGIN{n=split(z,arr,"\n"); for(i=1;i<=n;i++) if(arr[i]!="") seen[arr[i]]=1} !($1 in seen)' \
        | sort -t "$TAB" -k2,2
      } | awk NF
    )"
  else
    ordered="$(print -r -- "$labeled_all" | sort -t "$TAB" -k2,2)"
  fi

  # 5) fzf UI (stacked like zoxide)
  # Show: score + label, but return full path
  local selected
  selected="$(
    print -r -- "$ordered" \
    | fzf \
        --height=70% \
        --layout=reverse \
        --border \
        --info=inline \
        --no-separator \
        --prompt="repo(${sort_mode})> " \
        --query="${(j: :)q}" \
        --delimiter="$TAB" \
        --with-nth=3,2 \
        --preview='cd {1} && (git -c color.ui=always status -sb 2>/dev/null || true); echo; (ls -la | head -200)' \
        --preview-window='down,45%,wrap'
  )" || return 1

  local repo_path="${selected%%"$TAB"*}"
  [[ -n "$repo_path" ]] || return 1

  case "$mode" in
    print)
      print -r -- "$repo_path"
      ;;
    open)
      local ed="${EDITOR:-}"
      if [[ -z "$ed" ]]; then
        if command -v nvim >/dev/null 2>&1; then ed="nvim"
        elif command -v vim >/dev/null 2>&1; then ed="vim"
        else ed="vi"
        fi
      fi
      "$ed" "$repo_path"
      ;;
    *)
      # Make zoxide learn this selection (so frecent improves)
      if [[ $has_zoxide -eq 1 ]]; then
        zoxide add -- "$repo_path" 2>/dev/null
      fi
      cd -- "$repo_path" || return 1
      ;;
  esac
}

repo-link() {
  setopt local_options pipefail

  local dest=""
  local OPTIND=1 opt
  while getopts ":d:" opt; do
    case "$opt" in
      d) dest="$OPTARG" ;;
    esac
  done
  shift $((OPTIND-1))

  local target
  target="$(repo -p "$@")" || return 1
  [[ -n "$target" ]] || return 1

  if [[ -n "$dest" ]]; then
    [[ -d "$dest" ]] || { print -u2 "repo-link: destination not a directory: $dest"; return 1; }
  fi

  local link_path="${dest:+${dest%/}/}${target:t}"
  if [[ -e "$link_path" || -L "$link_path" ]]; then
    print -u2 "repo-link: '$link_path' already exists"
    return 1
  fi

  ln -s "$target" "$link_path" || return 1
  print -r -- "$link_path -> $target"
}
