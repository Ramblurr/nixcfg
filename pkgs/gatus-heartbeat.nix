{
  coreutils,
  curl,
  gnused,
  writeShellApplication,
}:
writeShellApplication {
  name = "gatus-heartbeat";
  runtimeInputs = [
    coreutils
    curl
    gnused
  ];
  text = ''
    usage() {
      echo "usage: gatus-heartbeat <systemd|report> --url URL --group GROUP --name NAME [--success true|false] [--error ERROR] [--duration DURATION] [--token-file FILE]" >&2
      exit 2
    }

    sanitize() {
      printf '%s' "$1" \
        | tr '[:upper:]' '[:lower:]' \
        | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//' -e 's|[/_., #+&]|-|g'
    }

    mode="''${1:-}"
    [[ "$mode" == "systemd" || "$mode" == "report" ]] || usage
    shift

    url=""
    group=""
    name=""
    success=""
    error=""
    duration=""
    token_file=""

    while (( $# > 0 )); do
      case "$1" in
        --url|--group|--name|--success|--error|--duration|--token-file)
          (( $# >= 2 )) || usage
          option="$1"
          value="$2"
          shift 2
          case "$option" in
            --url) url="$value" ;;
            --group) group="$value" ;;
            --name) name="$value" ;;
            --success) success="$value" ;;
            --error) error="$value" ;;
            --duration) duration="$value" ;;
            --token-file) token_file="$value" ;;
          esac
          ;;
        *) usage ;;
      esac
    done

    [[ -n "$url" && -n "$group" && -n "$name" ]] || usage

    if [[ "$mode" == "systemd" ]]; then
      if [[ "''${SERVICE_RESULT:-unknown}" == "success" ]]; then
        success=true
      else
        success=false
        error="''${SERVICE_RESULT:-unknown} (''${EXIT_CODE:-unknown}/''${EXIT_STATUS:-unknown})"
      fi
    fi
    [[ "$success" == "true" || "$success" == "false" ]] || usage

    token=""
    if [[ -n "$token_file" ]]; then
      token="$(cat "$token_file" 2>/dev/null || true)"
    elif [[ -n "''${CREDENTIALS_DIRECTORY:-}" && -s "$CREDENTIALS_DIRECTORY/gatus-token" ]]; then
      token="$(cat "$CREDENTIALS_DIRECTORY/gatus-token")"
    elif [[ -n "''${GATUS_EXTERNAL_TOKEN:-}" ]]; then
      token="$GATUS_EXTERNAL_TOKEN"
    elif [[ -n "''${BORGMATIC_GATUS_TOKEN:-}" ]]; then
      token="$BORGMATIC_GATUS_TOKEN"
    fi

    if [[ -z "$token" ]]; then
      echo "gatus-heartbeat: token is unavailable; result was not reported" >&2
      exit 0
    fi

    endpoint_key="$(sanitize "$group")_$(sanitize "$name")"
    request_args=(
      --fail
      --silent
      --show-error
      --connect-timeout 5
      --max-time 15
      --retry 3
      --retry-delay 1
      --retry-all-errors
      --get
      --request POST
      --header "Authorization: Bearer $token"
      --data-urlencode "success=$success"
    )
    [[ -z "$duration" ]] || request_args+=(--data-urlencode "duration=$duration")
    [[ -z "$error" ]] || request_args+=(--data-urlencode "error=$error")

    if ! curl "''${request_args[@]}" "''${url%/}/api/v1/endpoints/$endpoint_key/external"; then
      echo "gatus-heartbeat: failed to report result for $group/$name" >&2
    fi
  '';
}
