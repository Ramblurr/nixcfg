{ pkgs }:
let
  client = pkgs.writeText "james-haproxy-caddy-proxy-protocol-client.py" ''
    import socket
    import ssl
    import sys

    port = int(sys.argv[1])
    raw = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    raw.settimeout(5)
    raw.bind(("127.0.0.2", 0))
    raw.connect(("127.0.0.1", port))
    context = ssl.SSLContext(ssl.PROTOCOL_TLS_CLIENT)
    context.check_hostname = False
    context.verify_mode = ssl.CERT_NONE
    connection = context.wrap_socket(raw, server_hostname="james.example.test")
    connection.sendall(
        b"GET / HTTP/1.1\r\n"
        b"Host: james.example.test\r\n"
        b"Connection: close\r\n\r\n"
    )
    response = connection.recv(4096)
    connection.close()

    headers, body = response.split(b"\r\n\r\n", 1)
    assert headers.startswith(b"HTTP/1.1 200"), headers
    assert body == b"james.example.test|127.0.0.2", body
    print(body.decode())
  '';
in
pkgs.runCommand "james-haproxy-caddy-proxy-protocol-test"
  {
    nativeBuildInputs = [
      pkgs.caddy-with-security
      pkgs.haproxy
      pkgs.python3
    ];
  }
  ''
    set -euo pipefail

    export HOME="$TMPDIR/home"
    mkdir -p "$HOME"

    caddy_socket="$TMPDIR/james-caddy.sock"
    caddyfile="$TMPDIR/Caddyfile"
    haproxy_config="$TMPDIR/haproxy.cfg"
    caddy_log="$TMPDIR/caddy.log"
    haproxy_log="$TMPDIR/haproxy.log"

    printf '%s\n' \
      '{' \
      '  admin off' \
      '  auto_https disable_redirects' \
      '  servers {' \
      '    listener_wrappers {' \
      '      proxy_protocol {' \
      '        timeout 5s' \
      '        fallback_policy require' \
      '      }' \
      '      tls' \
      '    }' \
      '  }' \
      '}' \
      "" \
      'https://james.example.test {' \
      "  bind unix//$caddy_socket|0660" \
      '  tls internal' \
      '  respond "{http.request.tls.server_name}|{http.request.remote.host}"' \
      '}' > "$caddyfile"

    haproxy_port="$(${pkgs.python3}/bin/python3 -c 'import socket; s=socket.socket(); s.bind(("127.0.0.1", 0)); print(s.getsockname()[1]); s.close()')"

    printf '%s\n' \
      'global' \
      '  maxconn 128' \
      '  log stdout format raw local0' \
      "" \
      'defaults' \
      '  mode tcp' \
      '  timeout connect 5s' \
      '  timeout client 10s' \
      '  timeout server 10s' \
      "" \
      'frontend james_local' \
      "  bind 127.0.0.1:$haproxy_port" \
      '  default_backend caddy' \
      "" \
      'backend caddy' \
      "  server james-caddy $caddy_socket send-proxy" > "$haproxy_config"

    cleanup() {
      kill "''${haproxy_pid:-}" "''${caddy_pid:-}" 2>/dev/null || true
      wait "''${haproxy_pid:-}" "''${caddy_pid:-}" 2>/dev/null || true
    }
    trap cleanup EXIT

    caddy run --config "$caddyfile" --adapter caddyfile > "$caddy_log" 2>&1 &
    caddy_pid=$!

    for _ in $(seq 1 50); do
      test -S "$caddy_socket" && break
      kill -0 "$caddy_pid" 2>/dev/null || { cat "$caddy_log" >&2; exit 1; }
      sleep 0.1
    done
    test -S "$caddy_socket"

    haproxy -db -f "$haproxy_config" > "$haproxy_log" 2>&1 &
    haproxy_pid=$!

    for _ in $(seq 1 50); do
      if ${pkgs.python3}/bin/python3 -c 'import socket,sys; s=socket.socket(); s.settimeout(0.1); result=s.connect_ex(("127.0.0.1", int(sys.argv[1]))); s.close(); sys.exit(result)' "$haproxy_port"; then
        break
      fi
      kill -0 "$haproxy_pid" 2>/dev/null || { cat "$haproxy_log" >&2; exit 1; }
      sleep 0.1
    done

    ${pkgs.python3}/bin/python3 ${client} "$haproxy_port"
    touch "$out"
  ''
