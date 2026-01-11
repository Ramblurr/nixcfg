use std/log

let all_packages = (
  nix flake show --json
  | from json
  | get packages.x86_64-linux
  | columns
)

let packages_with_updatescript = (
  $all_packages
  | each {|p|
    let result = (^nix eval --raw $'.#($p).passthru.updateScript.type' | complete)
    if $result.exit_code == 0 { $p } else { null }
  }
  | compact
)

log info $'Found ($packages_with_updatescript | length) packages with update scripts'

for $package in $packages_with_updatescript {
  log info $'Updating ($package)'
  try {
    ^nix run $'.#($package).passthru.updateScript'
  } catch {|e|
    log warning $'Failed to run update script for ($package): ($e.msg)'
  }
}

log info 'Committing changes'

try {
  git add pkgs/
  git commit -m 'update(pkgs): Update sources of all downstream packages'
} catch {
  log warning 'No changes to commit'
}
