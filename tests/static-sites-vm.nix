{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  authorizedKeys = [ "ssh-ed25519 AAAATEST" ];
  sites = {
    preview = {
      domain = "preview.example.test";
      deployUser = "preview-publish";
      inherit authorizedKeys;
      spaFallback = true;
      cacheControlByMimeType = {
        "text/*" = "public, max-age=60";
        "text/html" = "public, max-age=0, must-revalidate";
        "text/css" = "public, max-age=120";
      };
      cacheControlByPath = {
        "/assets/*" = "public, max-age=31536000, immutable";
        "/assets/deep/*" = "public, max-age=300";
        "/assets/deep/config.json" = "no-store";
      };
    };
    other = {
      domain = "other.example.test";
      path = "/other/";
      inherit authorizedKeys;
      cacheControl = null;
      cacheControlByPath."/special.txt" = "no-store";
    };
    disabled = {
      enable = false;
      domain = "disabled.example.test";
      inherit authorizedKeys;
    };
  };
in
pkgs.testers.runNixOSTest {
  name = "static-sites";
  nodes.server = { config, ... }: {
    imports = [
      ../modules/services/static-sites.nix
      inputs.impermanence.nixosModules.impermanence
    ];
    modules.services.staticSites.sites = sites;
    environment.persistence."/persist".directories = [ "/var/lib/nixos" ];
    services.openssh.authorizedKeysFiles = [ "/run/test-keys/%u" ];
    networking.extraHosts = "127.0.0.1 preview.example.test other.example.test";
    networking.firewall.enable = false;
    environment.systemPackages = [ pkgs.curl ];
    services.caddy = {
      enable = true;
      extraConfig = lib.concatStringsSep "\n" (
        lib.mapAttrsToList (domain: handler: ''
          http://${domain} {
            ${handler}
          }
        '') config.modules.services.staticSites.handlersByDomain
      );
    };
  };
  testScript = ''
    start_all()
    server.wait_for_unit("caddy.service")
    server.wait_for_unit("sshd.service")
    curl = "curl -sS"

    with subtest("restricted SSH deployments"):
        server.succeed("mkdir -p /run/test-keys /tmp/site-upload/assets/deep; ssh-keygen -q -t ed25519 -N \"\" -f /root/deploy-key")
        server.succeed("cp /root/deploy-key.pub /run/test-keys/preview-publish; cp /root/deploy-key.pub /run/test-keys/disabled-deploy")
        server.succeed("printf 'preview-ok' > /tmp/site-upload/index.html; printf 'body {}' > /tmp/site-upload/style.css; printf 'note' > /tmp/site-upload/readme.txt; printf '{}' > /tmp/site-upload/assets/deep/config.json; printf '{}' > /tmp/site-upload/assets/deep/data.json; printf 'body {}' > /tmp/site-upload/assets/hashed.css")
        ssh = "ssh -o ConnectTimeout=5 -o ServerAliveInterval=5 -o ServerAliveCountMax=2 -i /root/deploy-key -o IdentitiesOnly=yes -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -o BatchMode=yes"
        rsync = f"rsync --timeout=20 -a --chmod=D755,F644 -e '{ssh}'"
        server.succeed(f"{rsync} --delete /tmp/site-upload/ preview-publish@localhost:.")
        server.succeed(f"{rsync} /tmp/site-upload/ preview-publish@localhost:/var/lib/static-sites/preview/")
        server.fail(f"{ssh} -n preview-publish@localhost id")
        server.fail(f"{ssh} -n disabled-deploy@localhost id")
        server.fail(f"{ssh} -n other-deploy@localhost id")
        server.fail(f"{rsync} /tmp/site-upload/ preview-publish@localhost:../other/")
        server.fail(f"{rsync} /tmp/site-upload/ preview-publish@localhost:/var/lib/static-sites/other/")
        server.fail(f"{rsync} preview-publish@localhost:. /tmp/stolen/")
        server.succeed("printf 'OUTSIDE_STATIC_SITE_PROBE' > /tmp/outside-static-site.txt; ln -s /tmp/outside-static-site.txt /tmp/site-upload/escape; printf 'touch /tmp/shell-escaped' > /tmp/site-upload/.bashrc")
        server.succeed(f"{rsync} /tmp/site-upload/ preview-publish@localhost:.")
        server.succeed(f"{rsync} /tmp/site-upload/ preview-publish@localhost:.")
        server.fail("test -e /tmp/shell-escaped")
        server.fail("su -s /bin/sh preview-publish -c 'touch /var/lib/static-sites/other/forbidden'")
        server.succeed("test $(getent passwd preview-publish | cut -d: -f6) = /var/empty")
        server.succeed("test -d /var/lib/static-sites/disabled")

    with subtest("cache precedence, validators, and SPA fallback"):
        server.wait_until_succeeds(f"{curl} -f http://preview.example.test/ | grep preview-ok")
        def headers(url):
            output = server.succeed(f"{curl} -D - -o /dev/null http://{url}")
            return {k.lower(): v.strip() for line in output.splitlines() if ': ' in line for k, v in [line.split(': ', 1)]}
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
            actual = headers("preview.example.test" + path)
            assert actual.get("cache-control") == policy, (path, actual)
        preview = headers("preview.example.test/")
        assert "etag" in preview and "last-modified" in preview, preview
        etag = preview["etag"]
        result = server.succeed(f"{curl} -o /dev/null -w '%{{http_code}}' -H 'If-None-Match: {etag}' http://preview.example.test/")
        assert result == "304", result
        server.succeed("su -s /bin/sh other-deploy -c 'echo other-ok > /var/lib/static-sites/other/index.html'")
        assert "cache-control" not in headers("other.example.test/other/")
        server.succeed("su -s /bin/sh other-deploy -c 'echo special > /var/lib/static-sites/other/special.txt'")
        assert headers("other.example.test/other/special.txt")["cache-control"] == "no-store"
        assert "cache-control" not in headers("other.example.test/other/missing")
        assert "cache-control" not in headers("other.example.test/other")
        body = server.succeed(f"{curl} -f http://preview.example.test/escape")
        assert body == "preview-ok", body

    with subtest("content survives reboot"):
        server.shutdown()
        server.start()
        server.wait_for_unit("caddy.service")
        server.wait_until_succeeds(f"{curl} -f http://preview.example.test/ | grep preview-ok")
  '';
}
