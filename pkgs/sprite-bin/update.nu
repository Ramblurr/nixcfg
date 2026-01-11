#!/usr/bin/env nu

let self = "pkgs/sprite-bin/package.nix"
let base_url = "https://sprites-binaries.t3.storage.dev"

# Fetch the latest RC version
print "Fetching latest RC version..."
let version = (http get $"($base_url)/client/rc.txt" | str trim)
print $"Latest version: ($version)"

mut updates = {}
$updates.version = $version

# Platform mappings: nix_platform -> binary_name
let platforms = {
  "aarch64-darwin": "darwin-arm64",
  "x86_64-darwin": "darwin-amd64",
  "aarch64-linux": "linux-arm64",
  "x86_64-linux": "linux-amd64"
}

# Fetch hash for each platform
for platform in ($platforms | transpose nix_platform binary_name) {
  let url = $"($base_url)/client/v($version)/sprite-($platform.binary_name).tar.gz"

  print $"Fetching hash for ($platform.nix_platform) \(($platform.binary_name)\)..."

  let hash = (
    ^nix-prefetch-url $url
    | str trim
    | nix hash to-sri --type sha256 $in
  )

  print $"  URL: ($url)"
  print $"  Hash: ($hash)"

  # Store the hash with the platform as key
  # Format: "aarch64-darwin.hash"
  $updates = ($updates | insert $"($platform.nix_platform).hash" $hash)
  # Format: "aarch64-darwin.url" (also update the URL with new version)
  $updates = ($updates | insert $"($platform.nix_platform).url" $url)
}

# Update the nix file
print "\nUpdating sprites-bin.nix..."
for update in ($updates | transpose key value) {
  (
    ast-grep run
    --pattern $'{($update.key) = $VALUE;}'
    --selector binding
    --rewrite $'($update.key) = "($update.value)";'
    --update-all
    $self
  )
}

print "Done!"
