{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  base = import ./james-caddy.nix { inherit inputs pkgs; };
  testSites = {
    preview = {
      domain = "preview.example.test";
      deployUser = "preview-publish";
      authorizedKeys = base.config.modules.services.staticSites.sites.epsilon.authorizedKeys;
      spaFallback = true;
      cacheControlByMimeType = {
        "text/*" = "public, max-age=60";
        "text/html" = "public, max-age=0, must-revalidate";
        "text/css" = "public, max-age=120";
        "image/*" = "public, max-age=600";
      };
      cacheControlByPath = {
        "/assets/*" = "public, max-age=31536000, immutable";
        "/assets/deep/*" = "public, max-age=300";
        "/assets/deep/config.json" = "no-store";
      };
    };
    other = {
      domain = "stardustdb.guide";
      path = "/other/";
      authorizedKeys = base.config.modules.services.staticSites.sites.epsilon.authorizedKeys;
      cacheControl = null;
      cacheControlByPath."/special.txt" = "no-store";
    };
    disabled = {
      enable = false;
      domain = "disabled.example.test";
      authorizedKeys = base.config.modules.services.staticSites.sites.epsilon.authorizedKeys;
    };
  };
  cfg =
    (base.evaluated.extendModules {
      modules = [ { modules.services.staticSites.sites = testSites; } ];
    }).config;
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
    imports = [
      ../hosts/james/web.nix
      ../modules/services/static-sites.nix
      inputs.impermanence.nixosModules.impermanence
    ];
    options = {
      repo.secrets = lib.mkOption { type = lib.types.attrs; };
      modules.zfs.datasets.properties = lib.mkOption { type = lib.types.attrs; };
    };
    config = {
      repo.secrets.global.domain.partner = "partner.example.test";
      modules.services.staticSites.sites = testSites;
      environment.persistence."/persist".directories = [ "/var/lib/nixos" ];
      services.openssh.authorizedKeysFiles = [
        "/etc/ssh/authorized_keys.d/%u"
        "/run/test-keys/%u"
      ];
      networking.extraHosts = "127.0.0.1 data-star.guide stardustdb.guide preview.example.test";
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
    try:
        james.wait_for_unit("caddy.service")
    except Exception:
        print(james.succeed("journalctl -u caddy --no-pager"))
        print(james.execute("${lib.getExe pkgs.caddy-with-security} validate --config /etc/caddy/caddy_config --adapter caddyfile 2>&1"))
        raise

    with subtest("tmpfiles creates a persistent deploy directory"):
        james.succeed("test -d /var/lib/static-web/stardustdb.guide/epsilon")
        james.succeed("su -s /bin/sh epsilon-deploy -c 'echo epsilon-ok > /var/lib/static-web/stardustdb.guide/epsilon/index.html'")
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

    with subtest("restricted SSH deployments"):
        james.wait_for_unit("sshd.service")
        james.succeed("mkdir -p /run/test-keys /tmp/site-upload/assets/deep; ssh-keygen -q -t ed25519 -N \"\" -f /root/deploy-key")
        james.succeed("cp /root/deploy-key.pub /run/test-keys/preview-publish; cp /root/deploy-key.pub /run/test-keys/disabled-deploy")
        james.succeed("printf 'preview-ok' > /tmp/site-upload/index.html; printf 'body {}' > /tmp/site-upload/style.css; printf 'note' > /tmp/site-upload/readme.txt; printf '{}' > /tmp/site-upload/assets/deep/config.json; printf '{}' > /tmp/site-upload/assets/deep/data.json; printf 'body {}' > /tmp/site-upload/assets/hashed.css")
        ssh = "ssh -o ConnectTimeout=5 -o ServerAliveInterval=5 -o ServerAliveCountMax=2 -i /root/deploy-key -o IdentitiesOnly=yes -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -o BatchMode=yes"
        rsync = f"rsync --timeout=20 -a --chmod=D755,F644 -e '{ssh}'"
        james.succeed(f"{rsync} --delete /tmp/site-upload/ preview-publish@localhost:.")
        james.succeed(f"{rsync} /tmp/site-upload/ preview-publish@localhost:/var/lib/static-sites/preview/")
        james.fail(f"{ssh} -n preview-publish@localhost id")
        james.fail(f"{ssh} -n disabled-deploy@localhost id")
        james.fail(f"{ssh} -n other-deploy@localhost id")
        james.fail(f"{rsync} /tmp/site-upload/ preview-publish@localhost:../other/")
        james.fail(f"{rsync} /tmp/site-upload/ preview-publish@localhost:/var/lib/static-sites/other/")
        james.fail(f"{rsync} preview-publish@localhost:. /tmp/stolen/")
        james.succeed("printf 'OUTSIDE_STATIC_SITE_PROBE' > /tmp/outside-static-site.txt; ln -s /tmp/outside-static-site.txt /tmp/site-upload/escape; printf 'touch /tmp/shell-escaped' > /tmp/site-upload/.bashrc")
        james.succeed(f"{rsync} /tmp/site-upload/ preview-publish@localhost:.")
        james.succeed(f"{rsync} /tmp/site-upload/ preview-publish@localhost:.")
        james.fail("test -e /tmp/shell-escaped")
        james.fail("su -s /bin/sh preview-publish -c 'touch /var/lib/static-sites/other/forbidden'")
        james.succeed("test $(getent passwd preview-publish | cut -d: -f6) = /var/empty")
        james.succeed("test -d /var/lib/static-sites/disabled")

    with subtest("cache precedence, validators, and SPA fallback"):
        james.wait_until_succeeds(f"{curl} -f https://preview.example.test/ | grep preview-ok")
        def headers(url, extra=""):
            output = james.succeed(f"{curl} -D - -o /dev/null {extra} https://{url}")
            return {k.lower(): v.strip() for line in output.splitlines() if ': ' in line for k, v in [line.split(': ', 1)]}
        prefix = "preview.example.test"
        expected = {
            "/": "public, max-age=0, must-revalidate",
            "/client/route": "public, max-age=0, must-revalidate",
            "/assets/missing.css": "public, max-age=0, must-revalidate",
            "/style.css": "public, max-age=120",
            "/readme.txt": "public, max-age=60",
            "/assets/hashed.css": "public, max-age=31536000, immutable",
            "/assets/deep/data.json": "public, max-age=300",
            "/assets/deep/config.json": "no-store",
        }
        for path, policy in expected.items():
            actual = headers(prefix + path)
            assert actual.get("cache-control") == policy, (path, actual)
        epsilon = headers("stardustdb.guide/epsilon/")
        assert epsilon["cache-control"] == "public, max-age=0, must-revalidate", epsilon
        assert "etag" in epsilon and "last-modified" in epsilon, epsilon
        etag = epsilon["etag"]
        result = james.succeed(f"{curl} -o /dev/null -w '%{{http_code}}' -H 'If-None-Match: {etag}' https://stardustdb.guide/epsilon/")
        assert result == "304", result
        james.succeed("su -s /bin/sh other-deploy -c 'echo other-ok > /var/lib/static-sites/other/index.html'")
        assert "cache-control" not in headers("stardustdb.guide/other/")
        james.succeed("su -s /bin/sh other-deploy -c 'echo special > /var/lib/static-sites/other/special.txt'")
        assert headers("stardustdb.guide/other/special.txt")["cache-control"] == "no-store"
        assert "cache-control" not in headers("stardustdb.guide/epsilon/missing")
        assert "cache-control" not in headers("stardustdb.guide/epsilon")
        body = james.succeed(f"{curl} -f https://preview.example.test/escape")
        assert body == "preview-ok", body

    with subtest("content survives reboot"):
        james.shutdown()
        james.start()
        james.wait_for_unit("caddy.service")
        james.wait_until_succeeds(f"{curl} -f https://stardustdb.guide/epsilon/ | grep epsilon-ok")
        james.wait_until_succeeds(f"{curl} -f https://preview.example.test/ | grep preview-ok")
  '';
}
