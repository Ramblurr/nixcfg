{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.microvm-guest;
  inherit (config.networking) hostName;
  inherit (config.system.build) toplevel;
  hostStore =
    !config.microvm.storeOnDisk && builtins.any (s: s.source == "/nix/store") config.microvm.shares;
  store =
    if cfg.writableStoreOverlay.zvolSize != null then
      "overlay"
    else if hostStore then
      "host"
    else
      null;

  stages = {
    overlay = ''
      requisites=$(nix-store --query --requisites ${toplevel})
      root_dir=$(ssh -n "$target" mktemp -d /nix/var/nix/gcroots/microvm-switch.XXXXXX)
      [[ "$root_dir" =~ ^/nix/var/nix/gcroots/microvm-switch\.[A-Za-z0-9._-]+$ ]] \
        || { echo "unexpected staging root: $root_dir" >&2; exit 1; }
      trap 'ssh -n "$target" rm -rf -- "$root_dir"' EXIT
      ssh "$target" xargs ln -s -t "$root_dir" <<< "$requisites"

      for attempt in 1 2 3; do
        missing=$(ssh "$target" xargs nix-store --check-validity --print-invalid <<< "$requisites")
        if [[ -n "$missing" ]]; then
          readarray -t paths <<< "$missing"
          echo "Copying ''${#paths[@]} store paths to $target"
          nix-store --export "''${paths[@]}" | ssh "$target" nix-store --import >/dev/null
        fi
        if ssh -n "$target" nix-store --check-validity ${toplevel} \
          && ssh -n "$target" "umask 077 && flock -s /nix/var/nix/gc.lock true" \
          && ssh -n "$target" nix-store --check-validity ${toplevel}; then
          break
        fi
        (( attempt < 3 )) || { echo "store paths keep disappearing from $target" >&2; exit 1; }
      done
    '';
    host = ''
      ssh "$target" nix-store --load-db < ${pkgs.closureInfo { rootPaths = [ toplevel ]; }}/registration
    '';
  };

  switcher = pkgs.writeShellApplication {
    name = "microvm-switch";
    text = ''
      target=''${1:?usage: microvm-switch root@<guest> [switch|boot|test|dry-activate]}
      action=''${2:-switch}
      case "$action" in
        switch|boot|test|dry-activate) ;;
        *) echo "unknown action: $action" >&2; exit 2 ;;
      esac
      remote_name=$(ssh -n "$target" cat /etc/hostname)
      [[ "$remote_name" == ${lib.escapeShellArg hostName} ]] \
        || { echo "refusing to switch $remote_name to ${hostName}" >&2; exit 1; }

      mkdir -p /nix/var/nix/gcroots/microvm
      ln -sfnT ${toplevel} /nix/var/nix/gcroots/microvm/staged-${hostName}
      ${stages.${store}}
      if [[ "$action" != boot ]]; then
        ssh -n "$target" ${toplevel}/bin/switch-to-configuration "$action"
      fi
      if [[ "$action" == switch || "$action" == boot ]]; then
        ssh -n "$target" nix-env -p /nix/var/nix/profiles/system --set ${toplevel}
      fi
    '';
  };
in
{
  config = lib.mkIf (cfg.enable && store != null) {
    system.switch.enable = true;
    microvm.deploy.sshSwitch = lib.mkForce switcher;
  };
}
