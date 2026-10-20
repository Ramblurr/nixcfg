#!/usr/bin/env bash
# Run on James with config and crowdsec binary paths from systemctl show crowdsec.
# cscli explain replays offline; it does not submit live decisions.
set -euo pipefail
config=${1:?CrowdSec config path required}
crowdsec=${2:?CrowdSec binary path required}
while read -r expected host method status uri; do
  log=$(jq -nc --arg host "$host" --arg method "$method" --arg uri "$uri" \
    --argjson status "$status" '{level:"info",ts:now,logger:"http.log.access",msg:"handled request",
    request:{remote_ip:"198.51.100.25",remote_port:"4242",client_ip:"198.51.100.25",
    proto:"HTTP/2.0",method:$method,host:$host,uri:$uri,
    headers:{"User-Agent":["Mozilla/5.0"]}},status:$status,size:123,duration:0.001}')
  output=$(cscli -c "$config" explain --crowdsec "$crowdsec" --type caddy --log "$log" -v)
  if ! grep -q 'create evt.Parsed.static_ressource : false' <<< "$output"; then
    printf '%s\n' "$output"
    exit 1
  fi
  actual=false
  if grep -q 'update evt.Parsed.static_ressource : false -> true' <<< "$output"; then
    actual=true
  fi
  if [[ "$actual" != "$expected" ]]; then
    printf 'FAIL: %s %s %s %s expected=%s actual=%s\n%s\n' "$host" "$method" "$status" "$uri" "$expected" "$actual" "$output"
    exit 1
  fi
  printf 'PASS: static=%s %s %s %s %s\n' "$actual" "$host" "$method" "$status" "$uri"
done <<'CASES'
true docs.outskirtslabs.com GET 200 /pagefind/fragment/en_a8b2395.pf_fragment
true docs.outskirtslabs.com GET 304 /pagefind/index/en_abc.pf_index
true docs.outskirtslabs.com GET 200 /pagefind/pagefind.en_abc.pf_meta
true docs.outskirtslabs.com GET 200 /pagefind/wasm.en.pagefind
false other.example.test GET 200 /pagefind/fragment/en_a8b2395.pf_fragment
false docs.outskirtslabs.com POST 200 /pagefind/fragment/en_a8b2395.pf_fragment
false docs.outskirtslabs.com GET 404 /pagefind/fragment/en_a8b2395.pf_fragment
false docs.outskirtslabs.com GET 200 /ol.busker/next/
false docs.outskirtslabs.com GET 200 /pagefind/fragment/not-an-asset
CASES
