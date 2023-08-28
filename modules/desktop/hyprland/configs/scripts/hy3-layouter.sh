#!/usr/bin/env bash
WORKSPACE=0
SLEEP_TIME=1
KILL_APPS=false
COMMAND=kitty
usage() {
    echo "A rather poor mimic of a center master layout."
    echo "Usage: $0 [options] command"
    echo "Options:"
    echo "  -w, --workspace <int>    Workspace to create the layout on"
    echo "  -s, --sleep <int>        Number of seconds to sleep after app launching"
    echo "  -k, --kill               Kill existing apps on the workspace"
    echo "  -h, --help               Display this help message"
    echo "Example:"
    echo "  $0 --workspace 2 --sleep 15 firefox"
}
while [[ $# -gt 0 ]]
do
    key="$1"
    case $key in
        -w|--workspace)
        WORKSPACE="$2"
        shift
        shift
        ;;
        -s|--sleep)
        SLEEP_TIME="$2"
        shift
        shift
        ;;
        -k|--kill)
        KILL_APPS=true
        shift
        ;;
        -h|--help)
        usage
        exit 0
        ;;
        *)
        COMMAND="$@"
        break
        ;;
    esac
done

H="hyprctl dispatch"

$H workspace $WORKSPACE


kitty &
$COMMAND &
sleep $SLEEP_TIME
kitty &
sleep 1

$H hy3:makegroup v
$H hy3:movefocus l
$H hy3:makegroup v
$H hy3:movefocus l
$H hy3:makegroup v
$H hy3:movefocus r
