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
  nodes.james = { ... }: {
    imports = [ ../hosts/james/web.nix ];
    options = {
      repo.secrets = lib.mkOption { type = lib.types.attrs; };
      modules.zfs.datasets.properties = lib.mkOption { type = lib.types.attrs; };
    };
    config = {
      repo.secrets.global.domain.partner = "partner.example.test";
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
  };
  testScript = ''
    start_all()
    james.wait_for_unit("haproxy.service")
    james.wait_for_unit("caddy.service")

    with subtest("tmpfiles creates a persistent deploy directory"):
        james.succeed("test -d /var/lib/static-web/stardustdb.guide/epsilon")
        james.succeed("su -s /bin/sh epsilon-deploy -c 'echo epsilon-ok > ~/index.html'")
        james.succeed("su -s /bin/sh caddy -c 'cat /var/lib/static-web/stardustdb.guide/epsilon/index.html'")
        james.succeed("chown root:root /var/lib/static-web/stardustdb.guide")
        james.succeed("systemd-tmpfiles --create --prefix=/var/lib/static-web/stardustdb.guide")
        assert james.succeed("stat -c %U:%G /var/lib/static-web/stardustdb.guide").strip() == "caddy:caddy"

    with subtest("real TLS-ALPN issuance through HAProxy"):
        james.succeed("curl --cacert /var/lib/pebble/cert.pem -fsS https://localhost:15000/roots/0 > /var/lib/pebble/root.pem")
        curl = "curl --cacert /var/lib/pebble/root.pem -sS"
        james.wait_until_succeeds(f"{curl} -f https://stardustdb.guide/epsilon/ | grep epsilon-ok", timeout=120)
        james.wait_until_succeeds(f"{curl} -I https://data-star.guide/ | grep -i 'location: https://data-star.dev/'")
        james.succeed(f"{curl} -I https://stardustdb.guide/ | grep -i 'location: https://stardustdb.com/'")
        headers = james.succeed(f"{curl} -I https://stardustdb.guide/epsilon")
        assert "HTTP/2 301" in headers and "location: /epsilon/" in headers.lower(), headers
        james.succeed(f"test $({curl} -o /dev/null -w '%{{http_code}}' https://stardustdb.guide/missing) = 404")

    with subtest("existing Unix-socket routes still work"):
        james.succeed("curl -ksSI --resolve code.work.example.test:443:127.0.0.1 https://code.work.example.test/ | grep -i 'location: https://code.example.test/work'")
        james.fail("curl --max-time 10 -ksS --resolve stardustdb.guide:8443:127.0.0.1 https://stardustdb.guide:8443/epsilon/")

    with subtest("content survives reboot"):
        james.shutdown()
        james.start()
        james.wait_for_unit("caddy.service")
        james.wait_until_succeeds(f"{curl} -f https://stardustdb.guide/epsilon/ | grep epsilon-ok")
  '';
}
