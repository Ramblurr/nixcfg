{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  cfg = (import ./james-caddy.nix { inherit inputs pkgs; }).config;
  pebbleConfig = pkgs.writeText "pebble.json" (
    builtins.toJSON {
      pebble = {
        listenAddress = "127.0.0.1:14000";
        managementListenAddress = "127.0.0.1:15000";
        certificate = "/var/lib/pebble/cert.pem";
        privateKey = "/var/lib/pebble/key.pem";
        httpPort = 80;
        tlsPort = 443;
      };
    }
  );
in
pkgs.testers.runNixOSTest {
  name = "james-guide-acme";
  nodes.james = _: {
    networking.extraHosts = "127.0.0.1 data-star.guide stardustdb.guide";
    networking.firewall.enable = false;
    environment.systemPackages = [
      pkgs.curl
      pkgs.openssl
    ];
    services.caddy = {
      enable = true;
      package = pkgs.caddy-with-security;
      globalConfig = ''
        auto_https disable_redirects
        skip_install_trust
        cert_issuer internal
        servers {
          protocols h1 h2
          strict_sni_host on
          listener_wrappers {
            proxy_protocol {
              timeout 5s
              fallback_policy require
            }
            tls
          }
        }
      '';
      extraConfig =
        lib.replaceStrings
          [ "dir https://acme-v02.api.letsencrypt.org/directory" ]
          [ "dir https://localhost:14000/dir\n        trusted_roots /var/lib/pebble/cert.pem" ]
          cfg.services.caddy.extraConfig;
    };
    systemd.services.caddy = {
      requires = [ "pebble.service" ];
      after = [ "pebble.service" ];
      serviceConfig.RuntimeDirectory = "caddy";
      serviceConfig.RuntimeDirectoryMode = "0750";
    };
    services.haproxy = {
      enable = true;
      config = cfg.services.haproxy.config;
    };
    users.users.haproxy.extraGroups = [ "caddy" ];
    systemd.services.pebble = {
      wantedBy = [ "multi-user.target" ];
      environment.PEBBLE_VA_NOSLEEP = "1";
      environment.PEBBLE_WFE_NONCEREJECT = "0";
      serviceConfig = {
        StateDirectory = "pebble";
        ExecStart = "${pkgs.pebble}/bin/pebble -config ${pebbleConfig}";
      };
      preStart = ''
        if ! test -e /var/lib/pebble/cert.pem; then
          ${pkgs.openssl}/bin/openssl req -x509 -newkey rsa:2048 -nodes \
            -keyout /var/lib/pebble/key.pem -out /var/lib/pebble/cert.pem \
            -days 1 -subj /CN=localhost -addext subjectAltName=DNS:localhost
        fi
      '';
    };
  };
  testScript = ''
    start_all()
    james.wait_for_unit("haproxy.service")
    james.wait_for_unit("caddy.service")
    james.succeed("curl --cacert /var/lib/pebble/cert.pem -fsS https://localhost:15000/roots/0 > /var/lib/pebble/root.pem")
    curl = "curl --cacert /var/lib/pebble/root.pem -sS"

    with subtest("TLS-ALPN issuance and redirects through HAProxy"):
        for host, path, destination in [
            ("data-star.guide", "/", "https://data-star.dev/"),
            ("stardustdb.guide", "/", "https://guide.stardustdb.com/"),
            ("stardustdb.guide", "/nested/page", "https://guide.stardustdb.com/"),
            ("stardustdb.guide", "/nested/page?query=value", "https://guide.stardustdb.com/"),
        ]:
            james.wait_until_succeeds(f"{curl} -I 'https://{host}{path}' > /tmp/headers", timeout=120)
            headers = [line.strip() for line in james.succeed("cat /tmp/headers").lower().splitlines()]
            assert "http/2 301" in headers and f"location: {destination}" in headers, headers

    with subtest("Unix-socket routing and mandatory PROXY protocol"):
        james.succeed("curl -ksSI --resolve code.work.example.test:443:127.0.0.1 https://code.work.example.test/ | grep -i 'location: https://code.example.test/work'")
        james.fail("curl --max-time 10 -ksS --resolve stardustdb.guide:8443:127.0.0.1 https://stardustdb.guide:8443/")
  '';
}
