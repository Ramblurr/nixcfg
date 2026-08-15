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
      ../modules/services/caddy-security-routes.nix
      ../modules/services/caddy-security.nix
      ../hosts/james/caddy.nix
      ../hosts/james/ingress-haproxy.nix
      inputs.impermanence.nixosModules.impermanence
      inputs.sops-nix.nixosModules.sops
      (
        { lib, ... }:
        {
          options = {
            hosts.james.ingress = {
              implementation = lib.mkOption {
                type = lib.types.enum [ "haproxy" ];
                default = "haproxy";
              };
              localBackend = lib.mkOption {
                type = lib.types.enum [
                  "nginx"
                  "caddy"
                ];
              };
            };
            repo.secrets = lib.mkOption { type = lib.types.attrs; };
          };
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
        services.nginx.enable = true;
        hosts.james.ingress = {
          implementation = "haproxy";
          localBackend = "caddy";
        };
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
  nginxBackendCfg =
    (evaluated.extendModules {
      modules = [
        { hosts.james.ingress.localBackend = lib.mkForce "nginx"; }
      ];
    }).config;
  caddy = cfg.services.caddy;
  caddyService = cfg.systemd.services.caddy;
  caddyConfig = caddy.configFile;
  expectedCertificateHosts = [
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
  failedAssertions = map (entry: entry.message) (lib.filter (entry: !entry.assertion) cfg.assertions);
in
assert lib.assertMsg (
  failedAssertions == [ ]
) "failed NixOS assertions: ${lib.concatStringsSep "; " failedAssertions}";
assert cfg.modules.services.caddy-security.enable;
assert !cfg.modules.services.caddy-security.loopbackListener;
assert
  cfg.modules.services.caddy-security.edge.bindAddress == "unix//run/caddy/james-ingress.sock|0660";
assert cfg.modules.services.caddy-security.edge.proxyProtocol;
assert
  cfg.modules.services.caddy-security.edge.protocols == [
    "h1"
    "h2"
  ];
assert cfg.modules.services.caddy-security.edge.certificateDomains == [ ];
assert cfg.modules.services.caddy-security.edge.certificateHosts == expectedCertificateHosts;
assert builtins.length expectedCertificateHosts == 23;
assert builtins.length (lib.unique expectedCertificateHosts) == 23;
assert caddy.enable;
assert caddy.package == pkgs.caddy-with-security;
assert !caddy.openFirewall;
assert lib.hasInfix "admin 127.0.0.1:2019" caddy.globalConfig;
assert lib.hasInfix "servers unix//run/caddy/james-ingress.sock|0660" caddy.globalConfig;
assert lib.hasInfix "listener_wrappers" caddy.globalConfig;
assert lib.hasInfix "proxy_protocol" caddy.globalConfig;
assert lib.hasInfix "fallback_policy require" caddy.globalConfig;
assert lib.hasInfix "strict_sni_host on" caddy.globalConfig;
assert !lib.hasInfix "servers :443" caddy.globalConfig;
assert !lib.hasInfix "http://:18080" caddy.extraConfig;
assert !lib.hasInfix "http://:8081" caddy.extraConfig;
assert lib.hasInfix "bind unix//run/caddy/james-ingress.sock|0660" caddy.extraConfig;
assert lib.hasInfix "output file /var/log/caddy/access.log" caddy.extraConfig;
assert lib.hasInfix "format json" caddy.extraConfig;
assert lib.hasInfix "respond 403" caddy.extraConfig;
assert lib.hasInfix "@plain_goaccess_allowed remote_ip 100.64.0.0/10" caddy.extraConfig;
assert lib.hasInfix "reverse_proxy unix//var/lib/casey.example.test/.run/site.sock"
  caddy.extraConfig;
assert lib.hasInfix "reverse_proxy http://127.0.0.1:1411" caddy.extraConfig;
assert lib.hasInfix "reverse_proxy http://127.0.0.1:1412" caddy.extraConfig;
assert lib.hasInfix "root * /var/lib/static-web/work.example.test/www" caddy.extraConfig;
assert lib.hasInfix "root * /var/lib/static-web/partner.example.test" caddy.extraConfig;
assert lib.hasInfix "path_regexp client_ip_latest" caddy.extraConfig;
assert lib.hasInfix "redir @client_ip_latest /ol.client-ip/0.1/{re.client_ip_latest.1} 302"
  caddy.extraConfig;
assert lib.hasInfix "redir /ol.client-ip/ /ol.client-ip/0.1/ 301" caddy.extraConfig;
assert lib.hasInfix "respond 421" caddy.extraConfig;
assert cfg.sops.templates.james-caddy-env.owner == "caddy";
assert cfg.sops.templates.james-caddy-env.group == "caddy";
assert cfg.sops.templates.james-caddy-env.mode == "0400";
assert
  cfg.sops.templates.james-caddy-env.content
  == "DESEC_API_TOKEN=${cfg.sops.placeholder.desec_api_token}";
assert lib.hasPrefix "/run/" caddy.environmentFile;
assert !lib.hasPrefix "/nix/store/" caddy.environmentFile;
assert builtins.elem "/var/lib/caddy" persistedDirectories;
assert
  caddyService.unitConfig.RequiresMountsFor == [
    "/var/lib/caddy"
    "/var/lib/goaccess"
    "/var/lib/static-web"
    "/var/lib/casey.example.test"
  ];
assert caddyService.serviceConfig.RuntimeDirectoryMode == "0750";
assert caddyService.serviceConfig.AmbientCapabilities == [ ];
assert caddyService.serviceConfig.CapabilityBoundingSet == [ ];
assert cfg.users.users.haproxy.extraGroups == [ "caddy" ];
assert builtins.elem "nginx" cfg.users.users.caddy.extraGroups;
assert builtins.elem "casey.example.test" cfg.users.users.caddy.extraGroups;
assert lib.hasInfix "server james-local /run/caddy/james-ingress.sock send-proxy"
  cfg.services.haproxy.config;
assert lib.hasInfix "server dewey dewey.prim.home.example.test:443" cfg.services.haproxy.config;
assert lib.hasInfix "server thingstead thingstead.moot.home.example.test:443"
  cfg.services.haproxy.config;
assert builtins.elem "caddy.service" cfg.systemd.services.haproxy.after;
assert builtins.elem "caddy.service" cfg.systemd.services.haproxy.wants;
assert !nginxBackendCfg.services.caddy.enable;
assert lib.hasInfix "server james-local /run/nginx/james-ingress.sock send-proxy"
  nginxBackendCfg.services.haproxy.config;
assert nginxBackendCfg.users.users.haproxy.extraGroups == [ "nginx" ];
assert builtins.elem "nginx.service" nginxBackendCfg.systemd.services.haproxy.after;
pkgs.runCommand "james-caddy-source-test"
  {
    nativeBuildInputs = [
      pkgs.caddy-with-security
      pkgs.jq
    ];
  }
  ''
    set -euo pipefail

    export DESEC_API_TOKEN=test-desec-token
    caddy adapt --adapter caddyfile --config ${caddyConfig} > "$TMPDIR/caddy.json"

    jq -e '.admin.listen == "127.0.0.1:2019"' "$TMPDIR/caddy.json"
    jq -e '
      [.apps.http.servers[].listen[]] == ["unix//run/caddy/james-ingress.sock|0660"]
    ' "$TMPDIR/caddy.json"
    jq -e '
      [.apps.http.servers[].listener_wrappers[].wrapper] == ["proxy_protocol", "tls"]
    ' "$TMPDIR/caddy.json"
    jq -e '
      [.. | objects | .host? // empty | .[]] | unique | length == 23
    ' "$TMPDIR/caddy.json"
    jq -e '
      [.. | objects | .host? // empty | .[]] | unique | all(startswith("*.") | not)
    ' "$TMPDIR/caddy.json"
    grep -Fq '"token":"{env.DESEC_API_TOKEN}"' "$TMPDIR/caddy.json"
    ! grep -Fq 'test-desec-token' "$TMPDIR/caddy.json"

    cat > "$TMPDIR/caddy-access.json" <<'JSON'
    {"level":"info","ts":1786816800.0,"logger":"http.log.access","msg":"handled request","request":{"remote_ip":"198.51.100.25","remote_port":"4242","client_ip":"198.51.100.25","proto":"HTTP/2.0","method":"GET","host":"work.example.test","uri":"/login","headers":{"User-Agent":["representative-agent"]}},"bytes_read":0,"duration":0.001,"size":123,"status":404}
    JSON
    jq -e '
      .logger == "http.log.access" and
      .request.client_ip == "198.51.100.25" and
      .request.method == "GET" and
      .request.host == "work.example.test" and
      .request.uri == "/login" and
      .request.headers["User-Agent"][0] == "representative-agent" and
      .status == 404
    ' "$TMPDIR/caddy-access.json"

    touch "$out"
  ''
