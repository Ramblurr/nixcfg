{ inputs, pkgs }:
let
  fakeOp = pkgs.writeShellScriptBin "op" ''
    set -eu
    test "''${OP_CONNECT_TOKEN:-}" = runtime-bootstrap-token
    test "$1" = read
    test "$2" = --no-newline
    test "$3" = op://home-ops-prod/gatus/borgmatic_external_endpoint_token
    printf %s heartbeat-test-token
  '';
in
pkgs.testers.runNixOSTest {
  name = "gatus-heartbeats";

  nodes.machine = {
    imports = [
      inputs.sops-nix.nixosModules.sops
      ../modules/services/onepassword-systemd-credentials.nix
      ../modules/site/gatus.nix
      ../modules/site/gatus-heartbeats.nix
      ../modules/site/gatus-heartbeats-onepassword.nix
    ];

    options = {
      repo.secrets = pkgs.lib.mkOption {
        type = pkgs.lib.types.attrs;
        default = { };
      };
      site.net = pkgs.lib.mkOption {
        type = pkgs.lib.types.attrs;
        default = { };
      };
    };

    config = {
      networking.hostName = "dewey";
      system.stateVersion = "26.05";
      networking.hosts."127.0.0.1" = [ "status.example.test" ];
      site.net.mgmt.hosts4.onepassword-connect = [ "192.0.2.22" ];
      repo.secrets.global.domain.home = "example.test";

      modules.services.onepassword-systemd-credentials = {
        package = fakeOp;
        bootstrapTokenFile = "/run/onepassword-provider-test-token";
      };

      systemd.services.prepare-provider-token = {
        serviceConfig.Type = "oneshot";
        script = ''
          umask 077
          printf %s runtime-bootstrap-token > /run/onepassword-provider-test-token
        '';
      };

      systemd.services.fake-gatus = {
        wantedBy = [ "multi-user.target" ];
        serviceConfig.Restart = "on-failure";
        script = ''
          mkdir -p /run/gatus-heartbeat-test
          ${pkgs.openssl}/bin/openssl req -x509 -newkey rsa:2048 -nodes \
            -keyout /run/gatus-heartbeat-test/key.pem \
            -out /run/gatus-heartbeat-test/cert.pem \
            -days 1 \
            -subj /CN=status.example.test \
            -addext subjectAltName=DNS:status.example.test 2>/dev/null
          ${pkgs.python3}/bin/python - <<'PY'
          import ssl
          from http.server import BaseHTTPRequestHandler, HTTPServer
          from pathlib import Path
          from urllib.parse import parse_qs, urlparse

          class Handler(BaseHTTPRequestHandler):
              def do_POST(self):
                  parsed = urlparse(self.path)
                  valid = (
                      parsed.path == "/api/v1/endpoints/infrastructure---operations_heartbeat-job-(dewey)/external"
                      and parse_qs(parsed.query) == {"success": ["true"]}
                      and self.headers.get("Authorization") == "Bearer heartbeat-test-token"
                  )
                  if valid:
                      Path("/run/gatus-heartbeat-vm-reported").touch()
                  self.send_response(200 if valid else 403)
                  self.send_header("Content-Length", "0")
                  self.end_headers()
              def log_message(self, *_args):
                  pass

          server = HTTPServer(("0.0.0.0", 443), Handler)
          context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
          context.load_cert_chain("/run/gatus-heartbeat-test/cert.pem", "/run/gatus-heartbeat-test/key.pem")
          server.socket = context.wrap_socket(server.socket, server_side=True)
          server.serve_forever()
          PY
        '';
      };

      systemd.services.heartbeat-job = {
        environment.CURL_CA_BUNDLE = "/run/gatus-heartbeat-test/cert.pem";
        serviceConfig = {
          Type = "oneshot";
          DynamicUser = true;
          StateDirectory = "heartbeat-test";
        };
        script = ''
          touch "$STATE_DIRECTORY/completed"
        '';
      };

      site.gatus.heartbeats.heartbeat-job = {
        service = "heartbeat-job";
        name = "Heartbeat Job";
        group = "Infrastructure & Operations";
        interval = "30h";
      };
    };
  };

  testScript = ''
    start_all()
    machine.wait_for_unit("multi-user.target")
    machine.succeed("systemctl start prepare-provider-token.service")
    machine.wait_for_unit("onepassword-credential-provider.socket")
    machine.wait_for_open_port(443)
    machine.succeed("systemctl start heartbeat-job.service")
    machine.succeed("test -e /var/lib/heartbeat-test/completed")
    machine.succeed("test -e /run/gatus-heartbeat-vm-reported")
    journal = machine.succeed("journalctl --no-pager -u heartbeat-job.service")
    assert "token is unavailable" not in journal
    assert "failed to report result" not in journal
    assert "heartbeat-test-token" not in journal
  '';
}
