use std/log

def main [
  ...packages: string  # Optional: specific packages to update
] {
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

  # Filter to specific packages if provided
  let packages_to_update = if ($packages | is-empty) {
    $packages_with_updatescript
  } else {
    $packages_with_updatescript | where {|p| $p in $packages}
  }

  log info $'Found ($packages_to_update | length) packages to update'

  for $package in $packages_to_update {
    log info $'Updating ($package)'
    try {
      ^nix run $'.#($package).passthru.updateScript'
    } catch {|e|
      log warning $'Failed to run update script for ($package): ($e.msg)'
    }
  }

  commit_changes
}

def commit_changes [] {

  log info 'Committing changes'

  try {
    git add pkgs/
    git commit -m 'update(pkgs): Update sources of all downstream packages'
  } catch {
    log warning 'No changes to commit'
  }
}
