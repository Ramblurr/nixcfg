#!/usr/bin/env bash

set -euo pipefail

FORCE=false
DATASET=""

usage() {
    echo "Usage: $0 [--force] <dataset>"
    echo
    echo "Releases zrepl abstractions and destroys a dataset and all children."
    echo "Dataset must match: data1/replication/mali/<something>"
    echo
    echo "Options:"
    echo "  --force    Actually execute commands (default is dry-run)"
    exit 1
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --force)
            FORCE=true
            shift
            ;;
        -h|--help)
            usage
            ;;
        -*)
            echo "Unknown option: $1" >&2
            usage
            ;;
        *)
            if [[ -n "$DATASET" ]]; then
                echo "Error: Multiple datasets specified" >&2
                usage
            fi
            DATASET="$1"
            shift
            ;;
    esac
done

if [[ -z "$DATASET" ]]; then
    echo "Error: No dataset specified" >&2
    usage
fi

# Validate dataset path: must be data1/replication/mali/<something>
if ! [[ "$DATASET" =~ ^data1/replication/mali/[^/]+.*$ ]]; then
    echo "Error: Dataset must match data1/replication/mali/<something>" >&2
    echo "Got: $DATASET" >&2
    exit 1
fi

run_cmd() {
    if [[ "$FORCE" == true ]]; then
        "$@"
    else
        echo "[dry-run] $*"
    fi
}

# Get all datasets (parent + children), sort by depth (deepest first)
mapfile -t DATASETS < <(zfs list -H -o name -r "$DATASET" | awk -F'/' '{print NF, $0}' | sort -rn | cut -d' ' -f2-)

if [[ ${#DATASETS[@]} -eq 0 ]]; then
    echo "No datasets found for: $DATASET" >&2
    exit 1
fi

if [[ "$FORCE" != true ]]; then
    echo "=== DRY RUN MODE (use --force to execute) ==="
    echo
fi

for ds in "${DATASETS[@]}"; do
    echo ">>> Processing: $ds"
    run_cmd  zrepl zfs-abstraction release-stale --fs "$ds"
    run_cmd  zrepl zfs-abstraction release-all --fs "$ds"
    run_cmd  zfs destroy -r "$ds"
    echo
done
