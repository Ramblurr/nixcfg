#!/bin/sh
set -eu
umask 077

SOURCE_DIR=$(CDPATH='' cd -- "$(dirname "$0")" && pwd)
# shellcheck source=/dev/null
. "$SOURCE_DIR/lib.sh"

suffix=${1-initial}
case $suffix in '' | *[!A-Za-z0-9._-]*) exit 64 ;; esac

temporary=$(mktemp "${TMPDIR:-/tmp}/zrepl-bundle-id.XXXXXX")
trap 'rm -f "$temporary"' EXIT HUP INT TERM
for relative in \
  bootstrap.sh lib.sh zrepl.yml rc.conf.d-zrepl managed-files \
  validation-datasets bundle.meta.in public-paths certificate-paths private-paths; do
  printf '%s  %s\n' "$(sha256_file "$SOURCE_DIR/$relative")" "$relative" >>"$temporary"
done
printf 'v1-%s-%s\n' "$(sha256_file "$temporary" | cut -c 1-16)" "$suffix"
