# Run against the private wrapper's evaluated Dewey config.
config:
let
  service = config.services.bookorbit;
  unit = config.systemd.services.bookorbit;
  domain = "books.${config.repo.secrets.home-ops.homeDomain}";
  library = "/mnt/mali/tank2/media/books";
in
assert config.home-ops.apps.bookorbit.enable;
assert service.enable;
assert service.package.version == "2.10.0";
assert service.environment.HOST == "127.0.0.1";
assert service.environment.DISABLE_LOCAL_AUTH == "true";
assert service.environment.APP_URL == "https://${domain}";
assert service.environment.LIBRARY_BROWSE_ROOT == library;
assert config.modules.services.bookorbit.domain == domain;
assert builtins.elem library unit.serviceConfig.ReadWritePaths;
assert !(builtins.elem library (unit.serviceConfig.ReadOnlyPaths or [ ]));
assert unit.serviceConfig.UMask == "0002";
assert unit.serviceConfig.ProtectSystem == "full";
assert builtins.elem "media" unit.serviceConfig.SupplementaryGroups;
assert builtins.elem "bookorbit-migrate.service" unit.requires;
assert builtins.length unit.serviceConfig.LoadCredential == 3;
assert
  config.modules.services.onepassword-systemd-credentials.consumers.bookorbit.BOOK_REQUEST_ENCRYPTION_KEY
  == "op://home-ops-prod/bookorbit/BOOK_REQUEST_ENCRYPTION_KEY";
assert !(service.environment ? BOOK_REQUEST_ENCRYPTION_KEY);
assert !(service.environment ? JWT_SECRET);
assert !(service.environment ? SETUP_BOOTSTRAP_TOKEN);
assert !(builtins.elem service.environment.PORT config.networking.firewall.allowedTCPPorts);
assert config.modules.services.caddy.routes.bookorbit.publicHost == domain;
assert
  config.modules.services.caddy.routes.bookorbit.upstream
  == "http://127.0.0.1:${toString service.environment.PORT}";
assert builtins.elem "bookorbit" config.services.postgresql.ensureDatabases;
assert
  config.modules.zfs.datasets.properties."rpool/encrypted/safe/svc/bookorbit".mountpoint
  == "/var/lib/bookorbit";
assert !config.home-ops.apps.calibre-web.enable;
assert !config.modules.services.calibre-web.enable;
assert !config.services.calibre-web.enable;
assert config.modules.zfs.datasets.enable;
assert !config.home-ops.apps.calibre.enable;
assert !config.modules.services.calibre.enable;
assert !(config.virtualisation.oci-containers.containers ? calibre);
assert !config.home-ops.apps.koreader-sync.enable;
assert !config.modules.services.koreader-sync.enable;
assert !(config.systemd.services ? podman-calibre);
assert !(config.systemd.services ? calibre-web);
assert !(config.systemd.services ? koreader-syncd);
assert config.fileSystems."/mnt/mali/tank2/media".fsType == "nfs";
assert !(config.modules.services.caddy.routes ? bookorbit-legacy);
assert builtins.all (
  endpoint:
  !(builtins.elem endpoint.name [
    "Calibre GUI"
    "Calibre Server"
    "Calibre Web"
    "Calibre Web Kobo"
    "KOReader Sync"
  ])
) config.site.gatus.endpoints;
assert builtins.any (
  endpoint: endpoint.name == "BookOrbit" && endpoint.url == "https://${domain}/api/v1/health"
) config.site.gatus.endpoints;
true
