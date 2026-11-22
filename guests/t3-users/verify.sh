#!/usr/bin/env bash
# Checks the t3-users guest: tailnet identities, grants, user scopes, that
# each user stays in its own network namespace, and that tenants reach the
# internet only through the egress exit node under their own rules.
# Run on quine as a user with sudo. Prints one PASS/FAIL line per check.
set -uo pipefail

TAILNET=${TAILNET:-tail91db2.ts.net}
GUEST=${GUEST:-172.20.20.25}
EGRESS=${EGRESS:-vsock/4246}
# VERIFY_GITHUB=0 skips the check that needs the live GitHub test token.
VERIFY_GITHUB=${VERIFY_GITHUB:-1}
fails=0
started=$(date +%s)

guest() {
  sudo -n ssh -i /var/lib/microvm-ssh/id_ed25519 -o IdentitiesOnly=yes \
    -o BatchMode=yes -o ConnectTimeout=5 -o HostKeyAlias=microvm-t3-users \
    -o UserKnownHostsFile=/etc/ssh/ssh_known_hosts "root@$GUEST" "$@"
}

egress() { sudo -n ssh "$EGRESS" "$@"; }

# A refused tailnet dial is dropped by policy, so the tunnel times out
# rather than answering 502. Anything other than 200 counts as refused.
# An empty result means the probe itself never ran.
refused() {
  case "$1" in
    200) echo 200 ;;
    "") echo probe-failed ;;
    *) echo refused ;;
  esac
}

check() {
  if [[ "$2" == "$3" ]]; then
    echo "PASS  $1"
  else
    echo "FAIL  $1 (expected $2, got $3)"
    fails=$((fails + 1))
  fi
}

# Runs a command as t3-<user> inside that user's network namespace.
in_ns() {
  local u=$1
  shift
  echo "ip netns exec t3-$u runuser -u t3-$u -- $*"
}

as_user() {
  local u=$1
  shift
  guest "$(in_ns "$u" "$@")" >/dev/null 2>&1
  verdict $?
}

# Runs a command as root in t3-<user>'s namespace. Root is captured like
# the tenant, but ICMP and other users' sockets are not.
as_user_root() {
  local u=$1
  shift
  guest "ip netns exec t3-$u $*" >/dev/null 2>&1
  verdict $?
}

verdict() {
  case $1 in
    0) echo ok ;;
    255) echo probe-failed ;;
    *) echo blocked ;;
  esac
}

# Prints the first bytes a TCP service sends, or probe-failed. The egress
# completes every captured handshake itself, so only data from the far end
# shows that a connection got through.
banner() {
  local u=$1 target=$2 out
  # Hold stdin open: an immediate EOF half-closes the stream and some
  # servers (GitHub's sshd) then hang up before sending a banner.
  out=$(guest "sleep 3 | $(in_ns "$u" socat -t5 -T5 - "TCP:$target,connect-timeout=5") 2>/dev/null | head -c 7")
  case $? in
    255) echo probe-failed ;;
    *) echo "${out:-none}" ;;
  esac
}

curl_cmd="curl -sS -o /dev/null --max-time 10"
quine="https://quine.$TAILNET"
proxy=http://127.0.0.1:1056
other() { [[ $1 == alice ]] && echo bob || echo alice; }

for u in alice bob; do
  tags=$(tailscale status --json |
    jq -r --arg h "t3-$u" '.Peer[] | select(.HostName == $h) | (.Tags // []) | join(",")')
  check "t3-$u node has tag:t3-$u" "tag:t3-$u" "$tags"
done

tunnel() {
  guest "$(in_ns "$1" curl -sS -o /dev/null -w "'%{http_connect}'" --max-time 10 \
    -p -x $proxy "http://$2/")" 2>/dev/null | head -c 3
}
check "alice tunnels to CLIProxyAPI" 200 "$(tunnel alice quine.$TAILNET:8317)"
check "alice tunnels to the inbox" 200 "$(tunnel alice quine.$TAILNET:43581)"
check "alice is refused quine SSH" refused "$(refused "$(tunnel alice quine.$TAILNET:22)")"
check "bob tunnels to CLIProxyAPI" 200 "$(tunnel bob quine.$TAILNET:8317)"
check "bob is refused the inbox" refused "$(refused "$(tunnel bob quine.$TAILNET:43581)")"
check "alice CLIProxyAPI request over TLS" ok "$(as_user alice $curl_cmd -x $proxy $quine:8317/)"

# Egress: captured flows and the proxy both leave through the exit node and
# get the caller's rules. httpbin.org and api.github.com are http rules: the
# egress decrypts them with its CA and swaps alice's placeholders.
check "alice TLS to registry.npmjs.org" ok "$(as_user alice $curl_cmd https://registry.npmjs.org/)"
check "alice gets httpbin.org through the egress CA" ok "$(as_user alice $curl_cmd https://httpbin.org/get)"

# Runs a command in t3-<user>'s login shell in its namespace, so it gets the
# placeholders and CA settings the tenant's sessions get.
login_sh() {
  guest "ip netns exec t3-$1 runuser -l t3-$1 -c $(printf %q "$2")" 2>/dev/null
}
status_as() { login_sh "$1" "curl -sS -o /dev/null -w '%{http_code}' --max-time 10 $2" || true; }
placeholder=TS_EGRESS_PLACEHOLDER_DUMMY_TOKEN
check "alice holds the DUMMY_TOKEN placeholder" $placeholder "$(login_sh alice 'printenv DUMMY_TOKEN')"
check "bob holds no DUMMY_TOKEN" "" "$(login_sh bob 'printenv DUMMY_TOKEN')"
check "bob holds no GITHUB_TOKEN" "" "$(login_sh bob 'printenv GITHUB_TOKEN')"
# The egress scrubs only response headers, never bodies, and httpbin
# echoes request headers in its body, so that echo shows the real value.
# No httpbin endpoint copies a request header into a response header, so
# header scrubbing is covered by the Go tests only. basic-auth answers 200
# only when the password equals the value in its path, without echoing it.
echo_body=$(login_sh alice 'curl -sS --max-time 10 -H "X-Dummy-Token: $DUMMY_TOKEN" https://httpbin.org/headers')
check "httpbin's body echo of alice's token is the real value" real \
  "$([[ $echo_body == *egress-dummy-value* && $echo_body != *$placeholder* ]] && echo real || echo "${echo_body:-probe-failed}")"
check "httpbin sees alice's real dummy value" 200 \
  "$(status_as alice '-u "u:$DUMMY_TOKEN" https://httpbin.org/basic-auth/u/egress-dummy-value')"
check "httpbin's basic-auth rejects a wrong password" 401 \
  "$(status_as alice '-u "u:$DUMMY_TOKEN" https://httpbin.org/basic-auth/u/wrong')"
check "egress refuses a placeholder in the path" 403 "$(status_as alice 'https://httpbin.org/anything/$DUMMY_TOKEN')"
check "egress refuses GITHUB_TOKEN on httpbin.org" 403 "$(status_as alice '-H "Authorization: Bearer $GITHUB_TOKEN" https://httpbin.org/get')"
check "egress refuses a Host that is not the SNI" 403 "$(status_as alice '-H "Host: example.com" https://httpbin.org/get')"
check "egress refuses alice's POST to api.github.com" 403 "$(status_as alice '-X POST https://api.github.com/user/repos')"
if [[ $VERIFY_GITHUB == 1 ]]; then
  check "alice's placeholder authenticates to api.github.com" 200 \
    "$(status_as alice '-H "Authorization: Bearer $GITHUB_TOKEN" https://api.github.com/user')"
else
  echo "SKIP  alice's placeholder authenticates to api.github.com (VERIFY_GITHUB=0)"
fi
check "bob is denied httpbin.org" blocked "$(as_user bob $curl_cmd https://httpbin.org/get)"
check "alice opens github.com:22" SSH-2.0 "$(banner alice github.com:22)"
ntp_at=$(date +%s)
ntp=$(guest bash -s <<EOF 2>/dev/null
{ printf '\x1b'; head -c 47 /dev/zero; } | $(in_ns alice socat -t5 -T5 - UDP:162.159.200.1:123) | wc -c
EOF
)
check "alice gets an NTP reply from 162.159.200.1" 48 "${ntp:-probe-failed}"
npm=$(guest "$(in_ns alice dig +short +time=3 +tries=1 registry.npmjs.org)" 2>/dev/null | grep -cE '^[0-9]+(\.[0-9]+){3}$')
check "alice resolves registry.npmjs.org" yes "$([[ ${npm:-0} -gt 0 ]] && echo yes || echo no)"
refusal=$(guest "$(in_ns alice dig +time=3 +tries=1 example.com)" 2>/dev/null |
  grep -oE 'status: [A-Z]+|EDE: [0-9]+' | paste -sd, -)
check "alice is refused example.com as blocked" "status: REFUSED,EDE: 15" "$refusal"
check "alice's proxy answers 403 for example.com" 403 \
  "$(guest "$(in_ns alice curl -sS -o /dev/null -w "'%{http_connect}'" --max-time 10 -x $proxy https://example.com/)" 2>/dev/null | head -c 3)"
check "bob TLS to registry.npmjs.org" ok "$(as_user bob $curl_cmd https://registry.npmjs.org/)"
check "bob is denied github.com" blocked "$(as_user bob $curl_cmd https://github.com/)"
check "bob's proxy reaches registry.npmjs.org" ok "$(as_user bob $curl_cmd -x $proxy https://registry.npmjs.org/)"
check "bob's proxy is denied github.com" blocked "$(as_user bob $curl_cmd -x $proxy https://github.com/)"

# The egress answers every captured port 53 itself, so the LAN probes use
# HTTP and ICMP. ts-plug's own uid is not captured, so its probes test the
# root namespace's forward rules.
for u in alice bob; do
  check "$u cannot reach 172.20.20.1" blocked "$(as_user $u $curl_cmd http://172.20.20.1/)"
  check "$u sends no ICMP to 172.20.20.1" blocked "$(as_user_root $u ping -c1 -W2 172.20.20.1)"
  check "$u cannot reach 1.1.1.1:443" blocked "$(as_user $u $curl_cmd https://1.1.1.1/)"
  check "ts-$u gets no DNS from 172.20.20.1" blocked \
    "$(as_user_root $u runuser -u ts-$u -- dig +time=2 +tries=1 @172.20.20.1 example.com)"
  check "ts-$u cannot reach 1.1.1.1:443" blocked \
    "$(as_user_root $u runuser -u ts-$u -- curl -sS -o /dev/null --max-time 5 https://1.1.1.1/)"
  check "ts-$u cannot reach egress SSH" blocked \
    "$(as_user_root $u runuser -u ts-$u -- socat -T3 - TCP:172.20.20.26:22,connect-timeout=3 '</dev/null')"
  check "ts-$u reaches control through the egress helper" ok \
    "$(as_user_root $u runuser -u ts-$u -- curl -sS -o /dev/null --max-time 10 -x http://172.20.20.26:3128 https://controlplane.tailscale.com/)"
  check "$u has no direct tailnet" blocked "$(as_user $u $curl_cmd $quine:8317/)"
  check "$u login shell has the proxy" $proxy \
    "$(guest "ip netns exec t3-$u runuser -l t3-$u -c 'printenv HTTPS_PROXY'" 2>/dev/null)"
done

# The first request waits while tsnet fetches the certificate.
for u in alice bob; do
  code=$(curl -sS -o /dev/null -w '%{http_code}' --max-time 60 "https://t3-$u.$TAILNET/" || echo none)
  check "t3-$u T3 answers on the tailnet" 200 "$code"
done

for u in alice bob; do
  uid=$(guest id -u t3-$u)
  user_env="env XDG_RUNTIME_DIR=/run/user/$uid DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/$uid/bus"
  scope="$user_env systemd-run --user --scope --quiet --slice=app-t3code-agents.slice --"
  check "$u can start a user scope" ok "$(as_user $u $scope true)"
  ifaces=$(guest "$(in_ns $u $user_env systemd-run --user --wait --pipe --quiet -- ip -o -4 addr)" 2>/dev/null |
    awk '{sub(/@.*/, "", $2); print $2}' | sort | paste -sd, -)
  check "$u user services run in its namespace" "lo,t3-$u-ns" "$ifaces"
  inv=$(guest systemctl show -p InvocationID --value t3-$u)
  state=$(guest "journalctl _SYSTEMD_INVOCATION_ID=$inv --no-pager -o cat | grep -o 'Agent scopes are o[nf]*' | tail -1")
  check "$u T3 has agent scopes on" "Agent scopes are on" "$state"
done

ns_addr() { guest "ip -n t3-$1 -o -4 addr show dev t3-$1-ns" | awk '{print $4}' | cut -d/ -f1; }
host_addr() { guest "ip -o -4 addr show dev t3-$1-host" | awk '{print $4}' | cut -d/ -f1; }

# Opens a listener as the owner in the owner's namespace, then connects to
# it from the owner (control) and from the prober. Prints
# isolated, reachable, or probe-failed when the control connection fails.
cross() {
  local owner=$1 prober=$2 bind=$3 target=$4 port=$5
  guest bash -s <<EOF 2>/dev/null |
$(in_ns "$owner" timeout 8 socat TCP-LISTEN:$port,bind=$bind,reuseaddr,fork "SYSTEM:'echo up'") >/dev/null 2>&1 &
listener=\$!
sleep 1
own=\$($(in_ns "$owner" socat -T3 - TCP:127.0.0.1:$port,connect-timeout=3) </dev/null 2>/dev/null)
seen=\$($(in_ns "$prober" socat -T3 - TCP:$target:$port,connect-timeout=3) </dev/null 2>/dev/null)
kill \$listener 2>/dev/null
echo "\$own/\$seen"
EOF
    awk '$0 == "up/" {r="isolated"} $0 == "up/up" {r="reachable"} END {print (r ? r : "probe-failed")}'
}

# Each owner gets its own ports, so a listener that outlives its check
# cannot answer the next one.
for u in alice bob; do
  o=$(other $u)
  addr=$(ns_addr $u)
  base=$([[ $u == alice ]] && echo 47000 || echo 47010)
  check "$o cannot reach $u's loopback listener" isolated "$(cross $u $o 127.0.0.1 127.0.0.1 $((base + 1)))"
  check "$o cannot reach $u's wildcard listener on loopback" isolated "$(cross $u $o 0.0.0.0 127.0.0.1 $((base + 2)))"
  check "$o cannot reach $u's wildcard listener at ${addr:-?}" isolated "$(cross $u $o 0.0.0.0 "${addr:-0.0.0.0}" $((base + 3)))"
done

for u in alice bob; do
  check "$u cannot reach guest SSH on svc" none "$(banner $u $GUEST:22)"
  haddr=$(host_addr $u)
  check "$u cannot reach guest SSH on its veth" none "$(banner $u "${haddr:-0.0.0.0}:22")"
done

# A tenant cannot stop its ts-plug unit (stopping it also tears the
# namespace down), but ts-plug can hang or die. Freeze it: nothing may
# leave the namespace while it does not answer.
for u in alice bob; do
  guest systemctl kill -s SIGSTOP ts-plug-$u
  check "$u has no TCP egress with ts-plug frozen" blocked "$(as_user $u curl -sS -o /dev/null --max-time 5 https://1.1.1.1/)"
  check "$u has no DNS with ts-plug frozen" blocked "$(as_user $u dig +time=3 +tries=1 registry.npmjs.org)"
  check "$u has no TLS egress with ts-plug frozen" blocked "$(as_user $u $curl_cmd https://registry.npmjs.org/)"
  guest systemctl kill -s SIGCONT ts-plug-$u
  up=$(guest "for i in \$(seq 30); do $(in_ns $u curl -sS -o /dev/null --max-time 5 https://registry.npmjs.org/) >/dev/null 2>&1 && echo up && break; sleep 1; done")
  check "ts-plug-$u answers again" up "$up"
done

# The egress logs a flow when it ends, a UDP flow after a minute idle.
# audit prints the last record's verdict and reason for caller
# tag:t3-<user> that matches the jq condition.
wait=$((ntp_at + 65 - $(date +%s)))
((wait > 0)) && sleep $wait
log=$(egress journalctl -u ts-egress -o cat --no-pager --since "@$started" 2>/dev/null)
audit() {
  [[ -n $log ]] || {
    echo probe-failed
    return
  }
  jq -Rr --arg t "tag:t3-$1" "fromjson? | select((.tags // []) | index(\$t)) | select($2) | [.verdict, .reason // empty] | join(\" \")" <<<"$log" |
    tail -1
}
check "egress passed alice's registry.npmjs.org" passthrough "$(audit alice '.sni == "registry.npmjs.org"')"
check "egress decrypted alice's httpbin.org" decrypt "$(audit alice '.msg == "flow" and .sni == "httpbin.org"')"
# requests prints status and credentials of the last decrypted request for
# caller tag:t3-<user> that matches the jq condition.
requests() {
  jq -Rr --arg t "tag:t3-$1" "fromjson? | select(.msg == \"request\") | select((.tags // []) | index(\$t)) | select($2) | [.status, ((.credentials // []) | join(\",\") | select(. != \"\")), .reason // empty] | map(tostring) | join(\" \")" <<<"$log" |
    tail -1
}
check "egress injected DUMMY_TOKEN for alice's basic-auth" "200 DUMMY_TOKEN" \
  "$(requests alice '.host == "httpbin.org" and (.path | startswith("/basic-auth/u/")) and (.path | endswith("/wrong") | not)')"
check "egress logged that path with the value scrubbed" "/basic-auth/u/$placeholder" \
  "$(jq -Rr 'fromjson? | select(.msg == "request" and .status == 200) | .path // empty' <<<"$log" | grep '^/basic-auth/u/' | tail -1)"
check "egress logged the placeholder-in-path refusal" "403 placeholder-in-url" \
  "$(requests alice '.path | startswith("/anything/")')"
check "egress never logged the dummy value" 0 "$(grep -c egress-dummy-value <<<"$log")"
check "egress decrypted nothing for bob" 0 \
  "$(jq -Rr --arg t tag:t3-bob 'fromjson? | select((.tags // []) | index($t)) | select(.msg == "request" or .verdict == "decrypt") | 1' <<<"$log" | wc -l)"
check "egress relayed alice's github.com:22" relay "$(audit alice '.proto == "tcp" and (.dst // "" | endswith(":22"))')"
check "egress relayed alice's NTP" relay "$(audit alice '.dst == "162.159.200.1:123"')"
check "egress denied alice's example.com lookup" deny "$(audit alice '(.question // "" | rtrimstr(".")) == "example.com"' | cut -d' ' -f1)"
# bob's lookup is refused first, so the deny shows up as a DNS question.
check "egress denied bob's github.com" deny "$(audit bob '.sni == "github.com" or .question == "github.com"' | cut -d' ' -f1)"

echo "failures: $fails"
exit $((fails > 0))
