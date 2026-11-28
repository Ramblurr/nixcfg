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
      CONTROL_BASE=/tmp
      [[ -w /dev/shm ]] && CONTROL_BASE=/dev/shm
      CONTROL_DIR=$(${coreutils}/bin/mktemp -d "$CONTROL_BASE/nixcfg-deploy.XXXXXX")
      SSH_OPTS=(-o ConnectTimeout=5 -o ServerAliveInterval=5 -o ServerAliveCountMax=2
        -o ControlMaster=auto -o ControlPath="$CONTROL_DIR/cm-%C" -o ControlPersist=300)
      SSH_ATTEMPT_SECONDS=25
      RECONNECT_SECONDS=120

      function close_control_masters() {
        local socket
        for socket in "$CONTROL_DIR"/cm-*; do
          [[ ! -S "$socket" ]] || ${coreutils}/bin/timeout --foreground 5 ssh -o ControlPath="$socket" -O exit deploy 2>/dev/null || true
        done
        rm -rf "$CONTROL_DIR"
      }
      trap close_control_masters EXIT

      function reset_control_master() {
        ${coreutils}/bin/timeout --foreground 5 ssh "''${SSH_OPTS[@]}" -O exit "$1" 2>/dev/null || true
      }

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

      # OpenSSH exits 255 for its own errors and timeout(1) exits 124 on expiry; retry both until the transport returns.
      function remote() {
        local ssh_host=$1 deadline=''${REMOTE_DEADLINE:-$((SECONDS + RECONNECT_SECONDS))} status
        shift
        while :; do
          status=0
          ${coreutils}/bin/timeout --foreground "$SSH_ATTEMPT_SECONDS" ssh "''${SSH_OPTS[@]}" "$ssh_host" -- "$@" \
            > "$CONTROL_DIR/stdout" || status=$?
          if [[ $status -eq 0 ]]; then
            cat "$CONTROL_DIR/stdout"
            return 0
          fi
          [[ ($status -eq 255 || $status -eq 124) && $SECONDS -lt $deadline ]] || return "$status"
          reset_control_master "$ssh_host"
          sleep 2
        done
      }

      function read_journal_since_cursor() {
        local ssh_host=$1 unit=$2 remote_command output last
        printf -v remote_command '%q ' journalctl --no-pager --quiet --output=cat --show-cursor --unit="$unit" \
          ''${JOURNAL_CURSOR:+"--after-cursor=$JOURNAL_CURSOR"}
        output=$(remote "$ssh_host" "$remote_command") || return $?
        last=''${output##*$'\n'}
        if [[ "$last" == "-- cursor: "* ]]; then
          JOURNAL_CURSOR=''${last#-- cursor: }
          output=''${output%"$last"}
        fi
        printf '%s' "$output"
      }

      function read_unit_state() {
        local ssh_host=$1 unit=$2 props key value
        local properties=LoadState,ActiveState,Result,ExecMainStatus,InactiveExitTimestampMonotonic
        state=()
        props=$(remote "$ssh_host" systemctl show --property="$properties" "$unit") || return 1
        while IFS='=' read -r key value; do
          [[ -z "$key" ]] || state[$key]=$value
        done <<< "$props"
        for key in ''${properties//,/ }; do
          [[ -n "''${state[$key]-}" ]] || return 1
        done
        [[ "''${state[LoadState]}" =~ ^(stub|loaded|not-found|bad-setting|error|merged|masked)$ ]]
      }

      function activate_remote() {
        local host=$1 ssh_host=$2 failure=$3 remote_command JOURNAL_CURSOR="" status=0
        local deadline=$((SECONDS + RECONNECT_SECONDS)) final_result final_detail
        shift 3
        local unit
        unit="nixcfg-deploy-$host-$(date +%s)-$$.service"
        local -A state=()
        printf -v remote_command '%q ' systemd-run --unit="$unit" --description="nixcfg deploy $ACTION $host" \
          --property=Type=oneshot --property=RemainAfterExit=yes \
          --property=StandardOutput=journal --property=StandardError=journal --setenv=PATH \
          --no-block -- "$@"
        ${coreutils}/bin/timeout --foreground "$SSH_ATTEMPT_SECONDS" ssh "''${SSH_OPTS[@]}" "$ssh_host" -- "$remote_command" \
          || status=$?
        case $status in
          0) ;;
          124|255) reset_control_master "$ssh_host" ;;
          *) die "$failure" ;;
        esac
        while :; do
          if REMOTE_DEADLINE=$deadline read_unit_state "$ssh_host" "$unit"; then
            deadline=$((SECONDS + RECONNECT_SECONDS))
            case "''${state[LoadState]}/''${state[ActiveState]}" in
              loaded/activating|loaded/reloading|loaded/deactivating) ;;
              loaded/inactive) [[ "''${state[InactiveExitTimestampMonotonic]}" == 0 ]] || break ;;
              *) break ;;
            esac
            REMOTE_DEADLINE=$SECONDS read_journal_since_cursor "$ssh_host" "$unit" || true
          elif [[ $SECONDS -ge $deadline ]]; then
            die "Could not read the state of $unit on $host for ''${RECONNECT_SECONDS}s; it may still be running there"
          fi
          sleep 2
        done
        final_result="''${state[ActiveState]}/''${state[Result]}/''${state[ExecMainStatus]}"
        final_detail="$unit: LoadState=''${state[LoadState]} ActiveState=''${state[ActiveState]} Result=''${state[Result]} ExecMainStatus=''${state[ExecMainStatus]}"
        deadline=$((SECONDS + RECONNECT_SECONDS))
        REMOTE_DEADLINE=$deadline remote "$ssh_host" journalctl --sync || true
        status=0
        REMOTE_DEADLINE=$deadline read_journal_since_cursor "$ssh_host" "$unit" || status=$?
        [[ $status -eq 0 ]] || echo "warning: failed to fetch final journal output for $unit on $host (exit $status)" >&2
        printf -v remote_command 'systemctl stop %q 2>/dev/null || true; systemctl reset-failed %q 2>/dev/null || true' \
          "$unit" "$unit"
        REMOTE_DEADLINE=$deadline remote "$ssh_host" "$remote_command" || true
        if ! REMOTE_DEADLINE=$deadline read_unit_state "$ssh_host" "$unit" \
          || [[ "''${state[LoadState]}" != not-found && "''${state[ActiveState]}" != inactive ]]; then
          echo "warning: failed to clean up $unit on $host" >&2
        fi
        [[ "$final_result" == active/success/0 ]] && return 0
        echo "$final_detail" >&2
        die "$failure"
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
          ssh "''${SSH_OPTS[@]}" "$ssh_host" -- true || die "Failed to authenticate to $host"
          NIX_SSHOPTS="''${SSH_OPTS[*]}" nix copy --substitute-on-destination --to "ssh://$ssh_host" "$store_path"
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
            ssh "''${SSH_OPTS[@]}" "root@''${SSH_TARGETS[$host]-$host}" -- nvd --color always diff /run/current-system "$store_path" \
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
          prev_system=$(ssh "''${SSH_OPTS[@]}" "$ssh_host" -- readlink -e /nix/var/nix/profiles/system)
          if [[ "$host" == "thinkpad1" ]]; then
            [[ -x "$store_path/thinkpad1-tpm-deploy" ]] \
              || die "Missing TPM deployment helper for $host"
            activate_remote "$host" "$ssh_host" "Failed TPM-aware activation of $host" \
              "$store_path/thinkpad1-tpm-deploy" "$ACTION" "$store_path"
          else
            if [[ "$ACTION" != "dry-activate" ]]; then
              ssh "''${SSH_OPTS[@]}" "$ssh_host" -- /run/current-system/sw/bin/nix-env --profile /nix/var/nix/profiles/system --set "$store_path" \
                || die "Failed to set system profile"
            fi
            activate_remote "$host" "$ssh_host" "Failed to activate $host" \
              "$store_path/bin/switch-to-configuration" "$ACTION"
          fi
          if [[ -n "$prev_system" ]]; then
            # nvd must be installed on the target system for this to work
            remote "$ssh_host" nvd --color always diff "$prev_system" "$store_path" || true
          fi
        fi
        time_next
        echo "[1;32m     Applied [m✅ [34m$host[m [90min ''${T_LAST}s[m"
      done
    '';
  };
in
deploy
