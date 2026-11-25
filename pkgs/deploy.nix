{
  bc,
  coreutils,
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
      localTargetHost=$(${coreutils}/bin/uname -n)
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

      [[ ''${#POSITIONAL_ARGS[@]} -ge 1 ]] || {
        show_help
        exit 1
      }

      ACTION="''${POSITIONAL_ARGS[0]}"
      case "$ACTION" in
        switch|boot|test|dry-activate|diff) ;;
        *) die "Unknown command: $ACTION (see deploy --help)" ;;
      esac

      [[ ''${#POSITIONAL_ARGS[@]} -ge 2 ]] || {
        show_help
        exit 1
      }

      printf '%s\n' "''${POSITIONAL_ARGS[@]:1}" | sort -u | readarray -t HOSTS

      resolve_targets "''${HOSTS[@]}" || die "Failed to resolve deployment targets"

      declare -A GUEST_PLANS=(
        [hot/diff]="diff"
        [hot/dry-activate]="activate"
        [hot/test]="activate"
        [hot/boot]="activate install"
        [hot/switch]="activate install"
        [restart/diff]="diff"
        [restart/boot]="install"
        [restart/switch]="install restart"
      )
      declare -A GUEST_REFUSALS=(
        [restart/test]="it has no hot switch, so it only runs a system by restarting onto it as the boot default"
        [restart/dry-activate]="its store is a read-only image and its system has no switch-to-configuration"
      )
      declare -A GUEST_STEP_BUILDS=(
        [diff]=system.build.toplevel
        [activate]=microvm.deploy.sshSwitch
        [install]=microvm.deploy.installOnHost
        [restart]=system.build.toplevel
      )

      declare -A TOPLEVEL_FLAKE_PATHS GUEST_HOSTS GUEST_SSH GUEST_PLAN GUEST_BUILT
      for host in "''${HOSTS[@]}"; do
        if [[ "$(target_field "$host" guest)" == true ]]; then
          kind="$(target_field "$host" activation)/$ACTION"
          [[ -n "''${GUEST_PLANS[$kind]-}" ]] \
            || die "Guest $host cannot $ACTION: ''${GUEST_REFUSALS[$kind]-unsupported}"
          GUEST_PLAN["$host"]=''${GUEST_PLANS[$kind]}
          guest_host=$(target_field "$host" host)
          [[ "$guest_host" =~ ^[a-zA-Z0-9][a-zA-Z0-9_-]*$ ]] || die "Invalid guest host: $guest_host"
          GUEST_HOSTS["$host"]="''${SSH_TARGETS[$guest_host]-$guest_host}"
          GUEST_SSH["$host"]=$(target_field "$host" guestSSH)
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
        build_steps=()
        if [[ -n "''${GUEST_PLAN[$host]-}" ]]; then
          build_targets=()
          for step in ''${GUEST_PLAN[$host]}; do
            [[ -n "''${GUEST_STEP_BUILDS[$step]-}" ]] || continue
            build_steps+=("$step")
            build_targets+=(".#nixosConfigurations.$host.config.''${GUEST_STEP_BUILDS[$step]}")
          done
        else
          build_targets=("''${TOPLEVEL_FLAKE_PATHS[$host]}")
        fi
        echo "[1;36m    Building [m📦 [34m$host[m"
        TOPLEVEL_STORE_PATHS["$host"]=$($BUILD_CMD --no-link --print-out-paths "''${OPTIONS[@]}" "''${build_targets[@]}") \
          || die "Failed to build ''${build_targets[*]}"
        if [[ ''${#build_steps[@]} -gt 0 ]]; then
          readarray -t built <<< "''${TOPLEVEL_STORE_PATHS[$host]}"
          [[ ''${#built[@]} -eq ''${#build_steps[@]} ]] || die "Expected ''${#build_steps[@]} build outputs for $host"
          for i in "''${!build_steps[@]}"; do
            GUEST_BUILT["$host/''${build_steps[$i]}"]=''${built[$i]}
          done
        fi
        time_next
        echo "[1;32m       Built [m✅ [34m$host[m [33m''${TOPLEVEL_STORE_PATHS["$host"]}[m [90min ''${T_LAST}s[m"
      done

      function guest_running() {
        local remote_command running
        printf -v remote_command '%q ' ssh -n -o BatchMode=yes "root@''${GUEST_SSH[$1]}" readlink -e /run/current-system
        running=$(ssh "root@''${GUEST_HOSTS[$1]}" -- "$remote_command") || die "Failed to read $1's running system"
        [[ "$running" =~ ^/nix/store/[a-z0-9]{32}-nixos-system-[A-Za-z0-9._+-]+$ ]] \
          || die "Unexpected running system for $1: $running"
        echo "$running"
      }

      function deploy_guest() {
        local host=$1 step remote_command
        local vm_host="root@''${GUEST_HOSTS[$host]}" guest="root@''${GUEST_SSH[$host]}"
        local -a host_paths=()
        local running
        for step in ''${GUEST_PLAN[$host]}; do
          [[ "$step" == install || -z "''${GUEST_BUILT[$host/$step]-}" ]] || host_paths+=("''${GUEST_BUILT[$host/$step]}")
        done
        echo "[1;36m     Copying [m➡️ [34m$host[m to [34m''${GUEST_HOSTS[$host]}[m"
        ssh "$vm_host" -- true || die "Failed to authenticate to ''${GUEST_HOSTS[$host]}"
        if [[ ''${#host_paths[@]} -gt 0 ]]; then
          nix copy --substitute-on-destination --to "ssh://$vm_host" "''${host_paths[@]}" \
            || die "Failed to copy $host's build to ''${GUEST_HOSTS[$host]}"
        fi
        for step in ''${GUEST_PLAN[$host]}; do
          case "$step" in
            diff)
              running=$(guest_running "$host")
              ssh "$vm_host" -- nix-store --check-validity "$running" \
                || die "$host runs $running, which ''${GUEST_HOSTS[$host]}'s store no longer holds"
              ssh "$vm_host" -- nvd --color always diff "$running" "''${GUEST_BUILT[$host/diff]}" \
                || die "Failed to diff $host"
              ;;
            activate)
              echo "[1;36m    Applying [m⚙️ [34m$host[m $ACTION over VSOCK from ''${GUEST_HOSTS[$host]}"
              printf -v remote_command '%q ' "''${GUEST_BUILT[$host/activate]}/bin/microvm-switch" "$guest" "$ACTION"
              ssh "$vm_host" -- "$remote_command" \
                || die "Failed to $ACTION $host; its runner on ''${GUEST_HOSTS[$host]} is unchanged"
              ;;
            install)
              echo "[1;36m  Installing [m⚙️ [34m$host[m runner as current on ''${GUEST_HOSTS[$host]}"
              "''${GUEST_BUILT[$host/install]}/bin/microvm-install-on-host" "$vm_host" \
                || die "Failed to install $host's runner on ''${GUEST_HOSTS[$host]}; rerun deploy $ACTION $host"
              ;;
            restart)
              running=$(guest_running "$host")
              if [[ "$running" == "''${GUEST_BUILT[$host/restart]}" ]]; then
                echo "$host already runs ''${GUEST_BUILT[$host/restart]}; not restarting it"
                continue
              fi
              echo "[1;36m  Restarting [m⚙️ microvm@$host on ''${GUEST_HOSTS[$host]}: $host has no hot switch"
              ssh "$vm_host" -- systemctl restart "microvm@$host.service" \
                || die "Failed to restart microvm@$host on ''${GUEST_HOSTS[$host]}"
              ;;
          esac
        done
        time_next
        [[ "$ACTION" == diff ]] || echo "[1;32m     Applied [m✅ [34m$host[m [90min ''${T_LAST}s[m"
      }

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
          deploy_guest "$host"
          continue
        fi
        if [[ "$ACTION" == diff ]]; then
          if [[ "$host" == "$localTargetHost" ]]; then
            nvd --color always diff /run/current-system "$store_path" \
              || die "Failed to diff $host"
          else
            ssh "root@''${SSH_TARGETS[$host]-$host}" -- nvd --color always diff /run/current-system "$store_path" \
              || die "Failed to diff $host"
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
