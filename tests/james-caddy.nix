{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  domains = {
    be = "binary.example.test";
    caseylink = "caseylink.example.test";
    "casey.link" = "casey.example.test";
    et = "truth.example.test";
    family = "family.example.test";
    home = "home.example.test";
    moot = "moot.example.test";
    ov = "vagrancy.example.test";
    partner = "partner.example.test";
    tailnet = "tailnet.example.test";
    work = "work.example.test";
    work2 = "legacy-work.example.test";
  };
  evaluated = inputs.nixpkgs.lib.nixosSystem {
    system = pkgs.stdenv.hostPlatform.system;
    modules = [
      ../modules/services/caddy.nix
      ../modules/services/static-sites.nix
      ../modules/services/onepassword-systemd-credentials.nix
      ../hosts/james/caddy.nix
      ../hosts/james/web.nix
      ../hosts/james/ingress-haproxy.nix
      ../hosts/james/goaccess.nix
      inputs.impermanence.nixosModules.impermanence
      inputs.sops-nix.nixosModules.sops
      (
        { lib, ... }:
        {
          options.repo.secrets = lib.mkOption { type = lib.types.attrs; };
          options.modules.zfs.datasets.properties = lib.mkOption { type = lib.types.attrs; };
        }
      )
      {
        nixpkgs.pkgs = pkgs;
        system.stateVersion = "26.05";
        boot.loader.grub.devices = [ "nodev" ];
        fileSystems."/" = {
          device = "none";
          fsType = "tmpfs";
        };
        networking.hostName = "james";
        repo.secrets = {
          global = {
            code = "https://code.example.test/personal";
            codeWork = "https://code.example.test/work";
            domain = domains;
            email.acme = "admin@example.test";
          };
          local = {
            atprotoDid = "did:plc:example";
          };
        };
        sops = {
          age.keyFile = "/tmp/age-key.txt";
          defaultSopsFile = ../hosts/james/secrets.sops.yaml;
        };
      }
    ];
  };
  cfg = evaluated.config;
  caddy = cfg.services.caddy;
  caddyService = cfg.systemd.services.caddy;
  caddyConfig = caddy.configFile;
  goaccessService = cfg.systemd.services.goaccess-report;
  goaccessTimer = cfg.systemd.timers.goaccess-report;
  expectedCertificateHosts = [
    "data-star.guide"
    "stardustdb.guide"
    domains.be
    "www.${domains.be}"
    domains."casey.link"
    "www.${domains."casey.link"}"
    "code.${domains."casey.link"}"
    domains.caseylink
    "www.${domains.caseylink}"
    "code.${domains.caseylink}"
    "code.${domains.work}"
    "docs.${domains.work}"
    domains.et
    "www.${domains.et}"
    "id.${domains.work}"
    "id.${domains.home}"
    domains.home
    "logs.${domains.work}"
    domains.ov
    "www.${domains.ov}"
    domains.work2
    "www.${domains.work2}"
    domains.work
    "www.${domains.work}"
    domains.partner
    "www.${domains.partner}"
  ];
  persistedDirectories = map (
    entry: if builtins.isString entry then entry else entry.directory
  ) cfg.environment.persistence."/persist".directories;
  caddyLines = lib.splitString "\n" caddy.extraConfig;
  docsMatcherIndex = lib.lists.findFirstIndex (
    line: lib.hasInfix "@plain_work_docs host docs.${domains.work}" line
  ) (builtins.length caddyLines) caddyLines;
  docsRouteLines = lib.drop docsMatcherIndex caddyLines;
  docsLineIndex =
    predicate: lib.lists.findFirstIndex predicate (builtins.length docsRouteLines) docsRouteLines;
  docsHandleIndex = docsLineIndex (line: lib.hasInfix "handle @plain_work_docs" line);
  docsWebhookIndex = docsLineIndex (line: lib.trim line == "handle /_deploy* {");
  docsHookIndex = docsLineIndex (line: lib.hasInfix "github-docs-hook.sock" line);
  docsCatchAllIndex = docsLineIndex (line: lib.trim line == "handle {");
  docsBackendIndex = docsLineIndex (line: lib.hasInfix "docs-site.sock" line);
  docsEndIndex = docsLineIndex (line: lib.trim line == "respond 421");
  failedAssertions = map (entry: entry.message) (lib.filter (entry: !entry.assertion) cfg.assertions);
in
assert lib.assertMsg (
  failedAssertions == [ ]
) "failed NixOS assertions: ${lib.concatStringsSep "; " failedAssertions}";
assert !caddy.openFirewall;
assert lib.hasInfix "respond 403" caddy.extraConfig;
assert lib.hasInfix "@plain_goaccess_allowed remote_ip 100.64.0.0/10" caddy.extraConfig;
assert docsMatcherIndex < builtins.length caddyLines;
assert docsHandleIndex < docsWebhookIndex;
assert docsWebhookIndex < docsHookIndex;
assert docsHookIndex < docsCatchAllIndex;
assert docsCatchAllIndex < docsBackendIndex;
assert docsBackendIndex < docsEndIndex;
assert lib.hasInfix "respond 421" caddy.extraConfig;
assert cfg.sops.templates.james-caddy-env.owner == "caddy";
assert cfg.sops.templates.james-caddy-env.group == "caddy";
assert cfg.sops.templates.james-caddy-env.mode == "0400";
assert
  cfg.sops.templates.james-caddy-env.content
  == "DESEC_API_TOKEN=${cfg.sops.placeholder.desec_api_token}";
assert lib.hasPrefix "/run/" caddy.environmentFile;
assert builtins.elem "/var/lib/caddy" persistedDirectories;
assert lib.all (path: builtins.elem path caddyService.unitConfig.RequiresMountsFor) [
  "/var/lib/caddy"
  "/var/lib/goaccess"
  "/var/lib/static-web"
  "/var/lib/casey.example.test"
];
assert caddyService.serviceConfig.RuntimeDirectory == "caddy";
assert caddyService.serviceConfig.RuntimeDirectoryMode == "0750";
assert caddyService.serviceConfig.AmbientCapabilities == [ ];
assert caddyService.serviceConfig.CapabilityBoundingSet == [ ];
assert cfg.users.users.haproxy.extraGroups == [ "caddy" ];
assert
  cfg.users.users.caddy.extraGroups == [
    "casey.example.test"
    "docs.work.example.test"
  ];
assert lib.hasInfix "server dewey dewey.prim.home.example.test:443" cfg.services.haproxy.config;
assert lib.hasInfix "server thingstead thingstead.moot.home.example.test:443"
  cfg.services.haproxy.config;
assert builtins.elem "caddy.service" cfg.systemd.services.haproxy.after;
assert builtins.elem "caddy.service" cfg.systemd.services.haproxy.wants;
assert builtins.elem cfg.environment.etc."haproxy.cfg".source
  cfg.systemd.services.haproxy.reloadTriggers;
assert lib.hasInfix "use_backend bk_dewey if { req.ssl_sni -i requests.home.example.test }"
  cfg.services.haproxy.config;
assert lib.hasInfix "use_backend bk_dewey if { req.ssl_sni -i photos.home.example.test }"
  cfg.services.haproxy.config;
assert lib.hasInfix "use_backend bk_dewey if { req.ssl_sni -i audiobookshelf.home.example.test }"
  cfg.services.haproxy.config;
assert builtins.elem "caddy.service" goaccessService.after;
assert goaccessService.unitConfig.ConditionPathExists == "/var/log/caddy/access.log";
assert goaccessService.serviceConfig.User == "caddy";
assert goaccessService.serviceConfig.Group == "caddy";
assert lib.hasInfix "--log-format=CADDY" goaccessService.serviceConfig.ExecStart;
assert goaccessTimer.wantedBy == [ "timers.target" ];
pkgs.runCommand "james-caddy-source-test"
  {
    passthru = {
      config = cfg;
      inherit evaluated;
    };
    nativeBuildInputs = [
      pkgs.caddy-with-security
      pkgs.jq
      pkgs.goaccess
    ];
  }
  ''
    set -euo pipefail

    export DESEC_API_TOKEN=test-desec-token
    caddy adapt --adapter caddyfile --config ${caddyConfig} > "$TMPDIR/caddy.json"

    jq -e '.admin.listen == "127.0.0.1:2019"' "$TMPDIR/caddy.json"
    jq -e '
      [.apps.http.servers[].listen[]] | sort == ["127.0.0.1:8443", "unix//run/caddy/james-ingress.sock|0660"]
    ' "$TMPDIR/caddy.json"
    jq -e '
      .apps.http.servers | all([.listener_wrappers[].wrapper] == ["proxy_protocol", "tls"])
    ' "$TMPDIR/caddy.json"
    jq -e --argjson expected '${builtins.toJSON expectedCertificateHosts}' '
      [.. | objects | .host? // empty | .[]] | unique == ($expected | sort)
    ' "$TMPDIR/caddy.json"
    jq -e '
      .apps.http.servers | all(
        .protocols == ["h1", "h2"]
        and .strict_sni_host == true
        and (.listener_wrappers | any(.wrapper == "proxy_protocol" and .fallback_policy == "REQUIRE"))
      )
    ' "$TMPDIR/caddy.json"
    jq -e '
      [.apps.tls.automation.policies[]
        | select(.subjects | index("data-star.guide") or index("stardustdb.guide"))] as $policies
      | ([$policies[].subjects[]] | sort) == ["data-star.guide", "stardustdb.guide"]
        and ($policies | all(.issuers | all(
          .module == "acme"
          and .challenges.bind_host == "127.0.0.1"
          and .challenges["tls-alpn"].alternate_port == 8443
          and .challenges.http.disabled == true
          and (.challenges.dns // null) == null
          and (.challenges["tls-alpn"].disabled // false) == false
        )))
    ' "$TMPDIR/caddy.json"
    grep -Fq '"token":"{env.DESEC_API_TOKEN}"' "$TMPDIR/caddy.json"
    ! grep -Fq 'test-desec-token' "$TMPDIR/caddy.json"

    cat > "$TMPDIR/caddy-access.json" <<'JSON'
    {"level":"info","ts":1786816800.0,"logger":"http.log.access","msg":"handled request","request":{"remote_ip":"198.51.100.25","remote_port":"4242","client_ip":"198.51.100.25","proto":"HTTP/2.0","method":"GET","host":"work.example.test","uri":"/login","headers":{"User-Agent":["representative-agent"]}},"bytes_read":0,"duration":0.001,"size":123,"status":404}
    JSON
    goaccess --no-global-config --log-format=CADDY --output="$TMPDIR/goaccess.json" \
      --json-pretty-print --no-progress "$TMPDIR/caddy-access.json"
    jq -e '.general.valid_requests == 1 and .general.failed_requests == 0' \
      "$TMPDIR/goaccess.json"

    touch "$out"
  ''
