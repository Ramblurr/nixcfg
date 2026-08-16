{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  secretFile = pkgs.writeText "caddy-test-secrets.yaml" "{}\n";
  evaluated = inputs.nixpkgs.lib.nixosSystem {
    system = pkgs.stdenv.hostPlatform.system;
    modules = [
      inputs.impermanence.nixosModules.impermanence
      inputs.sops-nix.nixosModules.sops
      ../modules/services/ingress-home-assistant.nix
      ../modules/services/ingress-octoprint.nix
      ../modules/services/calibre-web.nix
      ../modules/services/caddy.nix
      {
        options.repo.secrets = lib.mkOption {
          type = lib.types.attrs;
          default = { };
        };
        options.modules.zfs.datasets.properties = lib.mkOption {
          type = lib.types.attrs;
          default = { };
        };
      }
      {
        repo.secrets.home-ops.homeDomain = "example.test";
        modules.services.ingress-home-assistant.enable = true;
        modules.services.ingress-octoprint.enable = true;
        repo.secrets.global.nodes.mali.dataCIDR = "192.0.2.1";
        repo.secrets.home-ops.calibreWebPocketIdClientId = "calibre-client-id";
        modules.services.calibre-web = {
          enable = true;
          domain = "books.example.test";
          ports.http = 8083;
          mediaNfsShare = "books";
          user = {
            name = "books";
            uid = 991;
          };
          group = {
            name = "books";
            gid = 991;
          };
        };
        nixpkgs.pkgs = pkgs;
        system.stateVersion = "26.05";
        boot.loader.grub.devices = [ "nodev" ];
        fileSystems."/" = {
          device = "none";
          fsType = "tmpfs";
        };
        sops.defaultSopsFile = secretFile;
        sops.age.keyFile = "/tmp/age-key.txt";
        modules.services.caddy = {
          edge = {
            certificateDomains = [ "example.test" ];
            acmeEmail = "admin@example.test";
            sopsFile = secretFile;
            directWan = {
              enable = true;
              listenAddress = "192.0.2.1";
            };
          };
          auth.issuerURL = "https://id.example.test";
          routes = {
            jellyfin = {
              publicHost = "jelly.example.test";
              aliases = [ "media.example.test" ];
              upstream = "http://127.0.0.1:8096";
              requestBodyMaxSize = "10MB";
              flushInterval = "-1";
              directWan = true;
            };
            static = {
              publicHost = "static.example.test";
              root = "/srv/static";
              webSockets = false;
            };
          };
          protectedRoutes.alpha = {
            publicHost = "alpha.example.test";
            upstream = "http://127.0.0.1:8001";
            clientID = "alpha-client-id";
            requiredGroups = [ "books" ];
            bypassPathPrefixes = [ "/opds" ];
            identityHeaders = {
              Remote-User = "userinfo|preferred_username";
            };
          };
        };
      }
    ];
  };
  maliEvaluated = inputs.nixpkgs.lib.nixosSystem {
    system = pkgs.stdenv.hostPlatform.system;
    modules = [
      inputs.impermanence.nixosModules.impermanence
      inputs.sops-nix.nixosModules.sops
      ../modules/services/caddy.nix
      ../hosts/mali/caddy.nix
      ../hosts/mali/minio.nix
      ../hosts/mali/atticd.nix
      ../hosts/mali/ncps.nix
      {
        options.repo.secrets = lib.mkOption {
          type = lib.types.attrs;
          default = { };
        };
      }
      {
        nixpkgs.pkgs = pkgs;
        system.stateVersion = "26.05";
        boot.loader.grub.devices = [ "nodev" ];
        fileSystems."/" = {
          device = "none";
          fsType = "tmpfs";
        };
        sops.defaultSopsFile = secretFile;
        sops.age.keyFile = "/tmp/age-key.txt";
        repo.secrets.global = {
          domain.home = "example.test";
          email.acme = "admin@example.test";
        };
        _module.args.unstable = pkgs;
        _module.args.inputs = inputs;
        repo.secrets.global.localAtticSubstituter = "https://attic.example.test";
        repo.secrets.global.localAtticPublicKey = "attic-public-key";
        repo.secrets.global.nixCacheSubstituter = "https://cache.example.test";
        repo.secrets.global.nixCachePublicKey = "cache-public-key";
        repo.secrets.home-ops.ports.ncps = 3400;
      }
    ];
  };
  maliCfg = maliEvaluated.config;
  maliCaddy = maliCfg.services.caddy;
  cfg = evaluated.config;
  caddy = cfg.services.caddy;
  generatedConfig = caddy.configFile;
  failedAssertions = map (entry: entry.message) (lib.filter (entry: !entry.assertion) cfg.assertions);
  maliFailedAssertions = map (entry: entry.message) (
    lib.filter (entry: !entry.assertion) maliCfg.assertions
  );
  expectedEnvironment = ''
    DESEC_API_TOKEN=${cfg.sops.placeholder.desec_api_token}
    ALPHA_OIDC_CLIENT_SECRET=${cfg.sops.placeholder."alpha-oidc-client-secret"}
    ALPHA_SIGNING_KEY=${cfg.sops.placeholder."alpha-caddy-security-signing-key"}
    CALIBRE_WEB_OIDC_CLIENT_SECRET=${cfg.sops.placeholder."calibre-web-oidc-client-secret"}
    CALIBRE_WEB_SIGNING_KEY=${cfg.sops.placeholder."calibre-web-caddy-security-signing-key"}
  '';
in
assert
  failedAssertions == [ ]
  || throw "failed Caddy assertions: ${lib.concatStringsSep "; " failedAssertions}";
assert
  maliFailedAssertions == [ ]
  || throw "failed Mali Caddy assertions: ${lib.concatStringsSep "; " maliFailedAssertions}";
assert cfg.modules.services.caddy.routes.home-assistant.publicHost == "home.example.test";
assert cfg.modules.services.caddy.routes.octoprint.publicHost == "octoprint.example.test";
assert caddy.enable;
assert caddy.environmentFile == "/run/secrets/rendered/caddy-env";
assert cfg.sops.templates.caddy-env.owner == "caddy";
assert cfg.sops.templates.caddy-env.group == "caddy";
assert cfg.sops.templates.caddy-env.mode == "0400";
assert cfg.sops.templates.caddy-env.content == lib.removeSuffix "\n" expectedEnvironment;
assert builtins.elem 443 cfg.networking.firewall.allowedTCPPorts;
assert builtins.elem 8081 cfg.networking.firewall.allowedTCPPorts;
assert !builtins.elem 8443 cfg.networking.firewall.allowedTCPPorts;
assert builtins.elem 443 cfg.networking.firewall.allowedUDPPorts;
assert lib.hasInfix "servers :443" caddy.globalConfig;
assert lib.hasInfix "servers 192.0.2.1:8443" caddy.globalConfig;
assert lib.hasInfix "security {" caddy.globalConfig;
assert lib.hasInfix "@plain_home_assistant host home.example.test" caddy.extraConfig;
assert lib.hasInfix "@plain_octoprint host octoprint.example.test" caddy.extraConfig;
assert lib.hasInfix "@plain_jellyfin host jelly.example.test media.example.test" caddy.extraConfig;
assert lib.hasInfix "@protected_calibre_web host books.example.test" caddy.extraConfig;
assert lib.hasInfix "realm calibre-pocket-id" caddy.globalConfig;
assert lib.hasInfix "/oauth2/calibre-pocket-id" caddy.globalConfig;
assert lib.hasInfix "@protected_alpha host alpha.example.test" caddy.extraConfig;
assert lib.hasInfix "respond @unknown_host 421" caddy.extraConfig;
assert !lib.hasInfix "18080" caddy.globalConfig;
assert !lib.hasInfix "18080" caddy.extraConfig;
assert !lib.hasInfix "/healthz" caddy.extraConfig;
assert maliCaddy.enable;
assert
  maliCfg.modules.services.caddy.edge.certificateHosts == [
    "attic.mgmt.example.test"
    "attic.int.example.test"
    "nix-cache.int.example.test"
    "s3.data.example.test"
    "*.s3.data.example.test"
    "minio.data.example.test"
    "*.s3.mgmt.example.test"
    "minio.mgmt.example.test"
    "s3.mgmt.example.test"
  ];
assert maliCfg.modules.services.caddy.routes.attic.aliases == [ "attic.int.example.test" ];
assert
  maliCfg.modules.services.caddy.routes.minio-console.allowedRemoteIPs == [
    "10.9.8.0/23"
    "10.9.10.0/23"
  ];
assert lib.hasInfix "handle_path /minio/ui/*"
  maliCfg.modules.services.caddy.routes.minio-console.handlerConfig;
assert
  maliCfg.modules.services.caddy.routes.s3.requestHeaders.X-Real-IP == "{http.request.remote.host}";
assert maliCfg.modules.services.caddy.routes.s3.dialTimeout == "300s";
assert maliCfg.modules.services.caddy.routes.s3.flushInterval == "-1";
assert lib.hasInfix "remote_ip 10.9.8.0/23 10.9.10.0/23" maliCaddy.extraConfig;
assert !lib.hasInfix "18080" maliCaddy.globalConfig;
assert maliCaddy.configFile != null;
pkgs.runCommand "home-ops-caddy-test"
  {
    nativeBuildInputs = [
      pkgs.caddy-with-security
      pkgs.gnugrep
    ];
  }
  ''
    set -euo pipefail
    caddy version > "$TMPDIR/version"
    caddy build-info > "$TMPDIR/build-info"
    caddy list-modules > "$TMPDIR/modules"
    grep -Eq '^dep[[:space:]]+github\.com/greenpau/caddy-security[[:space:]]+v1\.1\.64([[:space:]]|$)' "$TMPDIR/build-info"
    grep -Eq '^dep[[:space:]]+github\.com/caddy-dns/desec[[:space:]]+v1\.1\.0([[:space:]]|$)' "$TMPDIR/build-info"
    grep -Fxq security "$TMPDIR/modules"
    grep -Fxq http.handlers.authenticator "$TMPDIR/modules"
    grep -Fxq http.authentication.providers.authorizer "$TMPDIR/modules"
    grep -Fxq dns.providers.desec "$TMPDIR/modules"
    ! grep -Fqi frankenphp "$TMPDIR/modules"
    caddy adapt --adapter caddyfile --config ${generatedConfig} > "$TMPDIR/caddy.json"
    caddy adapt --adapter caddyfile --config ${maliCaddy.configFile} > "$TMPDIR/mali-caddy.json"
    grep -Fq 'attic.mgmt.example.test' "$TMPDIR/mali-caddy.json"
    grep -Fq 'minio.data.example.test' "$TMPDIR/mali-caddy.json"
    grep -Fq 'nix-cache.int.example.test' "$TMPDIR/mali-caddy.json"
    grep -Fq '"jelly.example.test"' "$TMPDIR/caddy.json"
    grep -Fq '"media.example.test"' "$TMPDIR/caddy.json"
    grep -Fq '"alpha.example.test"' "$TMPDIR/caddy.json"
    grep -Fq '"header":"Remote-User"' "$TMPDIR/caddy.json"
    grep -Fq '{env.DESEC_API_TOKEN}' ${generatedConfig}
    grep -Fq '{env.ALPHA_OIDC_CLIENT_SECRET}' ${generatedConfig}
    grep -Fq '{env.ALPHA_SIGNING_KEY}' ${generatedConfig}
    ! grep -Fq '18080' ${generatedConfig}
    ! grep -Fq '/healthz' ${generatedConfig}
    touch "$out"
  ''
