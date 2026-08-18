{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  secretFile = pkgs.writeText "mali-garage-test-secrets.yaml" "{}\n";
  evaluated = inputs.nixpkgs.lib.nixosSystem {
    system = pkgs.stdenv.hostPlatform.system;
    modules = [
      inputs.impermanence.nixosModules.impermanence
      inputs.sops-nix.nixosModules.sops
      ../modules/services/caddy.nix
      ../modules/services/onepassword-systemd-credentials.nix
      ../modules/zfs-attrs.nix
      ../hosts/mali/caddy.nix
      ../modules/site/gatus.nix
      ../hosts/mali/garage.nix
      {
        options.repo.secrets = lib.mkOption {
          type = lib.types.attrs;
          default = { };
        };
        options.site.net = lib.mkOption {
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
        modules.services.onepassword-systemd-credentials = {
          enable = true;
          connectHost = "http://127.0.0.1:8080";
        };
        repo.secrets.global = {
          domain.home = "example.test";
          email.acme = "admin@example.test";
        };
        site.net = {
          mgmt = {
            subnet4 = "192.0.2.0/24";
            hosts4.mali = [ "192.0.2.3" ];
          };
          data.subnet4 = "198.51.100.0/24";
        };
      }
    ];
  };
  cfg = evaluated.config;
  settings = cfg.services.garage.settings;
  garageService = cfg.systemd.services.garage;
  datasets = cfg.modules.zfs.datasets.properties;
  garageRoutes = {
    inherit (cfg.modules.services.caddy.routes)
      garage-data
      garage-management
      ;
  };
  caddy = cfg.services.caddy;
  failedAssertions = map (entry: entry.message) (lib.filter (entry: !entry.assertion) cfg.assertions);
in
assert
  failedAssertions == [ ]
  || throw "failed Mali Garage assertions: ${lib.concatStringsSep "; " failedAssertions}";
assert map (check: check.name) cfg.site.gatus.endpoints == [ "Garage Management API" ];
assert cfg.services.garage.enable;
assert cfg.services.garage.package == pkgs.garage_2;
assert cfg.services.garage.package.version == "2.3.0";
assert
  settings == {
    metadata_dir = "/mnt/fast/services/garage/metadata";
    data_dir = "/mnt/tank2/services/garage/data";
    metadata_snapshots_dir = "/mnt/tank2/services/garage/metadata-snapshots";
    metadata_auto_snapshot_interval = "6h";
    metadata_fsync = true;
    db_engine = "sqlite";
    replication_factor = 1;
    consistency_mode = "consistent";
    rpc_bind_addr = "192.0.2.3:3901";
    rpc_public_addr = "192.0.2.3:3901";
    rpc_secret_file = "/run/secrets/garage/rpc-secret";
    s3_api = {
      api_bind_addr = "127.0.0.1:3900";
      s3_region = "us-east-1";
    };
    admin = {
      api_bind_addr = "192.0.2.3:3903";
      admin_token_file = "/run/secrets/garage/admin-token";
      metrics_token_file = "/run/secrets/garage/metrics-token";
      metrics_require_token = true;
    };
  };
assert !(settings.s3_api ? root_domain);
assert !(settings ? s3_web);
assert datasets."fast/services/garage".mountpoint == "none";
assert datasets."fast/services/garage/metadata".mountpoint == settings.metadata_dir;
assert datasets."tank2/services/garage".mountpoint == "/mnt/tank2/services/garage";
assert datasets."tank2/services/garage/data".mountpoint == settings.data_dir;
assert
  datasets."tank2/services/garage/metadata-snapshots".mountpoint == settings.metadata_snapshots_dir;
assert lib.all (name: cfg.sops.secrets.${name}.sopsFile == secretFile) [
  "garage/rpc-secret"
  "garage/admin-token"
  "garage/metrics-token"
];
assert lib.all
  (name: cfg.sops.secrets.${name}.owner == "garage" && cfg.sops.secrets.${name}.mode == "0400")
  [
    "garage/rpc-secret"
    "garage/admin-token"
    "garage/metrics-token"
  ];
assert lib.all
  (unit: builtins.elem unit garageService.requires && builtins.elem unit garageService.after)
  [
    "sops-install-secrets.service"
    "zfs-datasets.service"
  ];
assert
  garageService.unitConfig.AssertPathIsMountPoint == [
    settings.metadata_dir
    settings.data_dir
    settings.metadata_snapshots_dir
  ];
assert
  garageService.unitConfig.RequiresMountsFor == [
    settings.metadata_dir
    settings.data_dir
    settings.metadata_snapshots_dir
  ];
assert garageService.serviceConfig.User == "garage";
assert garageService.serviceConfig.Group == "garage";
assert !garageService.serviceConfig.DynamicUser;
assert lib.all (path: builtins.elem path garageService.serviceConfig.ReadWritePaths) [
  settings.metadata_dir
  settings.data_dir
  settings.metadata_snapshots_dir
];
assert garageService.postStart == "";
assert
  cfg.networking.firewall.interfaces.mgmt.allowedTCPPorts == [
    3901
    3903
  ];
assert !(builtins.elem 3900 cfg.networking.firewall.allowedTCPPorts);
assert !(builtins.elem 3901 cfg.networking.firewall.allowedTCPPorts);
assert !(builtins.elem 3903 cfg.networking.firewall.allowedTCPPorts);
assert garageRoutes.garage-data.publicHost == "garage.data.example.test";
assert garageRoutes.garage-data.allowedRemoteIPs == [ "198.51.100.0/24" ];
assert garageRoutes.garage-management.publicHost == "garage.mgmt.example.test";
assert garageRoutes.garage-management.allowedRemoteIPs == [ "192.0.2.0/24" ];
assert garageRoutes.garage-data.upstream == garageRoutes.garage-management.upstream;
assert garageRoutes.garage-data.upstream == "http://127.0.0.1:3900";
assert builtins.elem "garage.data.example.test" cfg.modules.services.caddy.edge.certificateHosts;
assert builtins.elem "garage.mgmt.example.test" cfg.modules.services.caddy.edge.certificateHosts;
assert lib.all (
  host: !(lib.hasInfix "garage" host) || !(lib.hasPrefix "*." host)
) cfg.modules.services.caddy.edge.certificateHosts;
assert lib.hasInfix "@plain_garage_data host garage.data.example.test" caddy.extraConfig;
assert lib.hasInfix "remote_ip 198.51.100.0/24" caddy.extraConfig;
assert lib.hasInfix "@plain_garage_management host garage.mgmt.example.test" caddy.extraConfig;
assert lib.hasInfix "remote_ip 192.0.2.0/24" caddy.extraConfig;
assert !cfg.services.minio.enable;
assert !(builtins.hasAttr "minio" cfg.systemd.services);
assert !(builtins.hasAttr "minio-root-credentials" cfg.sops.secrets);
assert !(builtins.elem "/var/lib/minio/" cfg.environment.persistence."/persist".directories);
assert lib.all (
  package: !(lib.hasPrefix "minio" (lib.getName package))
) cfg.environment.systemPackages;
assert !(builtins.elem "minio-2025-10-15T17-29-55Z" (pkgs.config.permittedInsecurePackages or [ ]));
assert !(cfg.modules.services.caddy.routes ? s3);
assert !(cfg.modules.services.caddy.routes ? minio-console);
assert lib.all (
  host: !(lib.hasInfix "minio" host) && !(lib.hasInfix "s3." host)
) cfg.modules.services.caddy.edge.certificateHosts;
pkgs.runCommand "mali-garage-test"
  {
    nativeBuildInputs = [ pkgs.caddy-with-security ];
  }
  ''
    set -euo pipefail
    sed "s#/var/log/caddy/access.log#$TMPDIR/access.log#g" ${caddy.configFile} > "$TMPDIR/Caddyfile"
    caddy validate --adapter caddyfile --config "$TMPDIR/Caddyfile"
    caddy adapt --adapter caddyfile --config ${caddy.configFile} > "$TMPDIR/caddy.json"
    grep -Fq 'garage.data.example.test' "$TMPDIR/caddy.json"
    grep -Fq 'garage.mgmt.example.test' "$TMPDIR/caddy.json"
    ! grep -Fq 'minio.data.example.test' "$TMPDIR/caddy.json"
    ! grep -Fq 's3.data.example.test' "$TMPDIR/caddy.json"
    touch "$out"
  ''
