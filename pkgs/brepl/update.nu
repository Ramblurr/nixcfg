#!/usr/bin/env nu

let self = "pkgs/brepl/package.nix"

# Fetch the latest release tag from GitHub
print "Fetching latest release from GitHub..."
let release = (
  ^gh api repos/licht1stein/brepl/releases/latest --jq '.tag_name'
  | str trim
)
print $"Latest release: ($release)"

# Prefetch the GitHub repo
print "Fetching hash..."
let prefetch = (
  ^nix-prefetch-github licht1stein brepl --rev $release
  | from json
)
let hash = $prefetch.hash
print $"  Hash: ($hash)"

# Update the nix file
print "\nUpdating package.nix..."

print $"  Updating rev to ($release)"
(
  ^ast-grep run
  --pattern '{rev = $VALUE;}'
  --selector binding
  --rewrite $'rev = "($release)";'
  --update-all
  $self
)

print $"  Updating hash"
(
  ^ast-grep run
  --pattern '{hash = $VALUE;}'
  --selector binding
  --rewrite $'hash = "($hash)";'
  --update-all
  $self
)

print "Done!"
