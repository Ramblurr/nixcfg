set -euo pipefail
mirror_root=$1
cache_root=$2
cgit_url=${3%/}
[[ -d "$mirror_root" ]]
mkdir -p "$cache_root/index" "$cache_root/repos"
status=0

while IFS= read -r -d '' repo; do
  relative=${repo#"$mirror_root/"}
  view="$cache_root/repos/$relative"
  mkdir -p "$view"
  # Private Git metadata supplies cgit links without changing Gickup's mirrors.
  # Objects and refs remain shared read-only; there is no second clone.
  if [[ ! -f "$repo/HEAD" ]]; then
    echo "Skipping unfinished mirror: $relative" >&2
    continue
  fi
  cp --remove-destination "$repo/HEAD" "$view/HEAD"
  for part in objects refs packed-refs shallow; do
    ln -sfnT "$repo/$part" "$view/$part"
  done
  git config --file "$view/config" core.bare true
  git config --file "$view/config" zoekt.name "${relative%.git}"
  git config --file "$view/config" zoekt.web-url "$cgit_url/$relative"
  git config --file "$view/config" zoekt.web-url-type cgit
  if ! git --git-dir="$view" rev-parse --verify HEAD >/dev/null 2>&1; then
    echo "Skipping incomplete or empty mirror: $relative" >&2
    continue
  fi
  if ! zoekt-git-index -index "$cache_root/index" -parallelism 2 \
    -require_ctags -submodules=false -branches HEAD "$view"; then
    echo "Indexing failed: $relative" >&2
    status=1
  fi
done < <(find "$mirror_root" -type d -name '*.git' -prune -print0)
exit "$status"
