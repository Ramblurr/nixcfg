{
  bc,
  jq,
  lib,
  writeShellApplication,
}:
let
  deploy = writeShellApplication {
    name = "deploy";
    text = ''
      set -euo pipefail
      shopt -s lastpipe # allow cmd | readarray
      localTargetHost=$(hostname)
      declare -A SSH_TARGETS=([addams]="addams-lan")

      function die() { echo "error: $*" >&2; exit 1; }
      ${import ./target-helpers.nix { inherit jq lib; }}


      function time_start() {
        T_START=$(date +%s.%N)
      }

      function time_next() {
        T_END=$(date +%s.%N)
        T_LAST=$(${bc}/bin/bc <<< "scale=1; ($T_END - $T_START)/1")
        T_START="$T_END"
      }

      USER_FLAKE_DIR=$(git rev-parse --show-toplevel 2>/dev/null || pwd) \
        || die "Could not determine current working directory. Something went very wrong."
      [[ -e "$USER_FLAKE_DIR/flake.nix" ]] \
        || die "Could not determine location of your project's flake.nix. Please run this at or below your main directory containing the flake.nix."
      cd "$USER_FLAKE_DIR"

      # Use nom build if available, otherwise fall back to nix build
      if command -v nom &> /dev/null; then
        BUILD_CMD="nom build"
      else
        BUILD_CMD="nix build"
      fi

      [[ $# -gt 0 ]] || {
        show_help
        exit 0
      }

      OPTIONS=()
      POSITIONAL_ARGS=()
      while [[ $# -gt 0 ]]; do
        case "$1" in
          "help"|"--help"|"-help"|"-h")
            show_help
            exit 0
            ;;

          -*) OPTIONS+=("$1") ;;
          *) POSITIONAL_ARGS+=("$1") ;;
        esac
        shift
      done

      [[ ''${#POSITIONAL_ARGS[@]} -ge 1 ]] \
        || die "Missing argument: <hosts...>"
      [[ ''${#POSITIONAL_ARGS[@]} -le 2 ]] \
        || die "Too many arguments given."

      tr , '\n' <<< "''${POSITIONAL_ARGS[0]}" | sort -u | readarray -t HOSTS
      ACTION="''${POSITIONAL_ARGS[1]-switch}"

      case "$ACTION" in
        switch|boot|test|dry-activate) ;;
        *) die "Unsupported action: $ACTION" ;;
      esac

      resolve_targets "''${HOSTS[@]}" || die "Failed to resolve deployment targets"

      declare -A TOPLEVEL_FLAKE_PATHS GUEST_HOSTS GUEST_IPS
      for host in "''${HOSTS[@]}"; do
        if [[ "$(target_field "$host" guest)" == true ]]; then
          [[ "$ACTION" == switch ]] || die "Guest $host only supports switch"
          guest_host=$(target_field "$host" host)
          [[ "$guest_host" =~ ^[a-zA-Z0-9][a-zA-Z0-9_-]*$ ]] || die "Invalid guest host: $guest_host"
          GUEST_HOSTS["$host"]="''${SSH_TARGETS[$guest_host]-$guest_host}"
          GUEST_IPS["$host"]=$(target_field "$host" guestIP)
          [[ "''${GUEST_IPS[$host]}" =~ ^[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+$ ]] || die "Invalid guest IPv4 address for $host"
          TOPLEVEL_FLAKE_PATHS["$host"]=".#nixosConfigurations.$host.config.microvm.deploy.rebuild"
        else
          TOPLEVEL_FLAKE_PATHS["$host"]=".#$(target_field "$host" buildAttribute)"
        fi
      done

      time_start
      #echo "[1;36m    Building [m📦 ''${#TOPLEVEL_FLAKE_PATHS[*]} configuration(s)"
      #nix build --no-link "''${TOPLEVEL_FLAKE_PATHS[@]}" "''${OPTIONS[@]}" \
      #  || die "Failed to build derivations"
      #time_next
      #echo "[1;32m       Built [m✅ ''${#TOPLEVEL_FLAKE_PATHS[*]} configuration(s) [90min ''${T_LAST}s[m"

      # Get outputs of all derivations (should be cached)
      declare -A TOPLEVEL_STORE_PATHS
      for host in "''${HOSTS[@]}"; do
        build_targets=("''${TOPLEVEL_FLAKE_PATHS[$host]}")
        if target_field "$host" guestSSH >/dev/null; then
          build_targets=(".#nixosConfigurations.$host.config.microvm.deploy.installOnHost"
                         ".#nixosConfigurations.$host.config.microvm.deploy.sshSwitch")
        fi
        echo "[1;36m    Building [m📦 [34m$host[m"
        TOPLEVEL_STORE_PATHS["$host"]=$($BUILD_CMD --no-link --print-out-paths "''${OPTIONS[@]}" "''${build_targets[@]}") \
          || die "Failed to get derivation path for $host from ''${TOPLEVEL_FLAKE_PATHS["$host"]}"
        time_next
        echo "[1;32m       Built [m✅ [34m$host[m [33m''${TOPLEVEL_STORE_PATHS["$host"]}[m [90min ''${T_LAST}s[m"
      done

      for host in "''${HOSTS[@]}"; do
        [[ -z "''${GUEST_HOSTS[$host]-}" ]] || continue
        store_path="''${TOPLEVEL_STORE_PATHS["$host"]}"
        if [[ "$host" == "$localTargetHost" ]]; then
          echo "[1;36m     Copying [m[34m$host[m skipped; target is local"
        else
          ssh_target="''${SSH_TARGETS[$host]-$host}"
          ssh_host="root@$ssh_target"
          echo "[1;36m     Copying [m➡️ [34m$host[m"
          # Establish the configured SSH master before Nix creates its own private socket.
          ssh "$ssh_host" -- true || die "Failed to authenticate to $host"
          nix copy --substitute-on-destination --to "ssh://$ssh_host" "$store_path"
        fi
        time_next
        echo "[1;32m      Copied [m✅ [34m$host[m [90min ''${T_LAST}s[m"
      done

      for host in "''${HOSTS[@]}"; do
        store_path="''${TOPLEVEL_STORE_PATHS["$host"]}"
        if [[ -n "''${GUEST_HOSTS[$host]-}" ]]; then
          ssh_host="root@''${GUEST_HOSTS[$host]}"
          if guest_ssh=$(target_field "$host" guestSSH); then
            echo "Deploying guest $host via $ssh_host -> $guest_ssh"
            readarray -t guest_helpers <<< "$store_path"
            [[ ''${#guest_helpers[@]} -eq 2 ]] || die "Expected two built guest helpers for $host"
            installer="''${guest_helpers[0]}"
            switcher="''${guest_helpers[1]}"
            # Authenticate before changing the host's installed guest runner.
            # SSH runs on the VM host so its identity and pinned keys are used.
            printf -v remote_command '%q ' ssh -o BatchMode=yes "root@$guest_ssh" true
            ssh "$ssh_host" -- "$remote_command" \
              || die "VSOCK preflight failed for $host; not falling back to network SSH"
            nix copy --to "ssh://$ssh_host" "$switcher" \
              || die "Failed to copy guest switch helper for $host"
            "$installer/bin/microvm-install-on-host" "$ssh_host" \
              || die "Failed to install guest $host on its host"
            printf -v remote_command '%q ' "$switcher/bin/microvm-switch" "root@$guest_ssh"
            ssh "$ssh_host" -- "$remote_command" \
              || die "Failed to switch guest $host over VSOCK"
          else
            echo "Deploying guest $host on ''${GUEST_HOSTS[$host]} (''${GUEST_IPS[$host]})"
            # Keep the upstream network route for guests without VSOCK SSH.
            "$store_path/bin/microvm-rebuild" "$ssh_host" "root@''${GUEST_IPS[$host]}" \
              || die "Failed to deploy guest $host"
          fi
          continue
        fi
        echo "[1;36m    Applying [m⚙️ [34m$host[m"
        if [[ "$host" == "$localTargetHost" ]]; then
          prev_system=$(readlink -e /nix/var/nix/profiles/system)
          if [[ "$host" == "thinkpad1" ]]; then
            [[ -x "$store_path/thinkpad1-tpm-deploy" ]] \
              || die "Missing TPM deployment helper for $host"
            sudo "$store_path/thinkpad1-tpm-deploy" "$ACTION" "$store_path" \
              || die "Failed TPM-aware activation of $host"
          else
            if [[ "$ACTION" != "dry-activate" ]]; then
              sudo /run/current-system/sw/bin/nix-env --profile /nix/var/nix/profiles/system --set "$store_path" \
                || die "Failed to set system profile"
            fi
            sudo "$store_path"/bin/switch-to-configuration "$ACTION" \
              || die "Failed to activate $host"
          fi
          if [[ -n "$prev_system" ]]; then
            nvd --color always diff "$prev_system" "$store_path" || true
          fi
        else
          ssh_target="''${SSH_TARGETS[$host]-$host}"
          ssh_host="root@$ssh_target"
          prev_system=$(ssh "$ssh_host" -- readlink -e /nix/var/nix/profiles/system)
          if [[ "$host" == "thinkpad1" ]]; then
            [[ -x "$store_path/thinkpad1-tpm-deploy" ]] \
              || die "Missing TPM deployment helper for $host"
            ssh "$ssh_host" -- "$store_path/thinkpad1-tpm-deploy" "$ACTION" "$store_path" \
              || die "Failed TPM-aware activation of $host"
          else
            if [[ "$ACTION" != "dry-activate" ]]; then
              ssh "$ssh_host" -- /run/current-system/sw/bin/nix-env --profile /nix/var/nix/profiles/system --set "$store_path" \
                || die "Failed to set system profile"
            fi
            ssh "$ssh_host" -- "$store_path"/bin/switch-to-configuration "$ACTION" \
              || die "Failed to activate $host"
          fi
          if [[ -n "$prev_system" ]]; then
            # nvd must be installed on the target system for this to work
            ssh "$ssh_host" -- nvd --color always diff "$prev_system" "$store_path" || true
          fi
        fi
        time_next
        echo "[1;32m     Applied [m✅ [34m$host[m [90min ''${T_LAST}s[m"
      done
    '';
  };
in
deploy
