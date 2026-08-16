{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  secretFile = pkgs.writeText "debord-monitoring-test-secrets.yaml" "{}\n";
  evaluated = inputs.nixpkgs.lib.nixosSystem {
    system = pkgs.stdenv.hostPlatform.system;
    modules = [
      inputs.sops-nix.nixosModules.sops
      ../modules/telemetry/prometheus.nix
      ../modules/telemetry/thanos.nix
      ../hosts/debord/prometheus.nix
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
        nixpkgs.pkgs = pkgs;
        networking.hostName = "debord";
        repo.secrets = {
          global.domain.home = "example.test";
          home-ops.ports.zrepl-metrics = 9811;
        };
        system.stateVersion = "26.05";
        boot.loader.grub.devices = [ "nodev" ];
        fileSystems."/" = {
          device = "none";
          fsType = "tmpfs";
        };
        sops.defaultSopsFile = secretFile;
        sops.age.keyFile = "/tmp/age-key.txt";
      }
    ];
  };
  cfg = evaluated.config;
  alertmanagerCfg = cfg.services.prometheus.alertmanager;
  backupRules = ../hosts/debord/backup-alerts.rules.yml;
  backupRulesTest = ./debord-backup-alerts.test.yml;
  alertmanagerConfig =
    (pkgs.formats.yaml { }).generate "alertmanager-null.yml"
      alertmanagerCfg.configuration;
  pipelinePrometheusConfig = (pkgs.formats.yaml { }).generate "alertmanager-pipeline-prometheus.yml" {
    global = {
      evaluation_interval = "100ms";
      scrape_interval = "1s";
    };
    rule_files = [ ./alertmanager-pipeline.rules.yml ];
    alerting.alertmanagers = cfg.services.prometheus.alertmanagers;
    scrape_configs = [ ];
  };
  failedAssertions = map (entry: entry.message) (lib.filter (entry: !entry.assertion) cfg.assertions);
  scrapeJobs = lib.listToAttrs (
    map (job: lib.nameValuePair job.job_name job.static_configs) cfg.services.prometheus.scrapeConfigs
  );
  scrapeTargets = lib.mapAttrs (
    _: staticConfigs: lib.concatMap (static: static.targets) staticConfigs
  ) scrapeJobs;
  objectStoreSecret = cfg.sops.secrets.thanos_sidecar_object_storage_configuration;
  objectStorePath = objectStoreSecret.path;
  thanosUnits = map (name: cfg.systemd.services.${name}) [
    "thanos-sidecar"
    "thanos-store"
    "thanos-query"
    "thanos-compact"
  ];
  objectStoreReaders = map (name: cfg.systemd.services.${name}) [
    "thanos-sidecar"
    "thanos-store"
    "thanos-compact"
  ];
in
assert
  failedAssertions == [ ]
  || throw "failed monitoring assertions: ${lib.concatStringsSep "; " failedAssertions}";
assert cfg.services.prometheus.enable;
assert cfg.services.prometheus.ruleFiles == [ backupRules ];
assert
  cfg.services.prometheus.alertmanagers == [
    {
      static_configs = [ { targets = [ "127.0.0.1:9093" ]; } ];
    }
  ];
assert alertmanagerCfg.enable;
assert alertmanagerCfg.checkConfig;
assert alertmanagerCfg.listenAddress == "127.0.0.1";
assert alertmanagerCfg.port == 9093;
assert !alertmanagerCfg.openFirewall;
assert alertmanagerCfg.extraFlags == [ "--cluster.listen-address=" ];
assert alertmanagerCfg.configText == null;
assert
  alertmanagerCfg.configuration == {
    route.receiver = "null";
    receivers = [ { name = "null"; } ];
  };
assert !(builtins.elem 9093 cfg.networking.firewall.allowedTCPPorts);
assert builtins.elem "multi-user.target" cfg.systemd.services.alertmanager.wantedBy;
assert cfg.systemd.services.alertmanager.serviceConfig.DynamicUser;
assert cfg.systemd.services.alertmanager.serviceConfig.StateDirectory == "alertmanager";
assert cfg.services.prometheus.stateDir == "prometheus2";
assert cfg.services.prometheus.retentionTime == "1d";
assert builtins.elem "--storage.tsdb.min-block-duration=2h" cfg.services.prometheus.extraFlags;
assert builtins.elem "--storage.tsdb.max-block-duration=2h" cfg.services.prometheus.extraFlags;
assert cfg.services.prometheus.globalConfig.external_labels == { prometheus = "debord"; };
assert
  cfg.modules.zfs.datasets.properties."rpool/encrypted/safe/svc/prometheus".mountpoint
  == "/var/lib/prometheus2";
assert builtins.elem "var-lib-prometheus2.mount" cfg.systemd.services.prometheus.requires;
assert builtins.elem "zfs-datasets.service" cfg.systemd.services.prometheus.requires;
assert lib.all (unit: !(builtins.elem "var-lib-prometheus2.mount" unit.requires)) thanosUnits;
assert lib.all (unit: !(builtins.elem "zfs-datasets.service" unit.requires)) thanosUnits;
assert
  map (job: job.job_name) cfg.services.prometheus.scrapeConfigs == [
    "node"
    "smartd"
    "zfs"
    "nut"
    "ipmi"
    "zrepl_dewey"
    "zrepl_debord"
    "zrepl_mali"
    "prometheus"
    "alertmanager"
    "thanos-sidecar"
    "thanos-store"
    "thanos-query"
    "thanos-compact"
  ];
assert
  scrapeTargets == {
    node = [
      "dewey.mgmt.example.test:9100"
      "debord.mgmt.example.test:9100"
      "mali.mgmt.example.test:9100"
      "addams.mgmt.example.test:9100"
    ];
    smartd = [
      "dewey.mgmt.example.test:9633"
      "debord.mgmt.example.test:9633"
      "mali.mgmt.example.test:9633"
      "addams.mgmt.example.test:9633"
    ];
    zfs = [
      "dewey.mgmt.example.test:9134"
      "debord.mgmt.example.test:9134"
      "mali.mgmt.example.test:9134"
      "addams.mgmt.example.test:9134"
    ];
    nut = [ "mali.mgmt.example.test:9199" ];
    ipmi = [ "mali.mgmt.example.test:9290" ];
    zrepl_dewey = [ "dewey.mgmt.example.test:9811" ];
    zrepl_debord = [ "debord.mgmt.example.test:9811" ];
    zrepl_mali = [ "mali.mgmt.example.test:9811" ];
    prometheus = [ "127.0.0.1:9090" ];
    alertmanager = [ "127.0.0.1:9093" ];
    thanos-sidecar = [ "127.0.0.1:10902" ];
    thanos-store = [ "127.0.0.1:10904" ];
    thanos-query = [ "127.0.0.1:10906" ];
    thanos-compact = [ "127.0.0.1:10907" ];
  };
assert
  cfg.services.thanos.compact.retention == {
    resolution-raw = "30d";
    resolution-5m = "1y";
    resolution-1h = "2y";
  };
assert !cfg.services.thanos.compact.downsampling.disable;
assert builtins.elem ''--retention.resolution-raw="30d"'' cfg.services.thanos.compact.arguments;
assert builtins.elem ''--retention.resolution-5m="1y"'' cfg.services.thanos.compact.arguments;
assert builtins.elem ''--retention.resolution-1h="2y"'' cfg.services.thanos.compact.arguments;
assert builtins.elem "--wait" cfg.services.thanos.compact.arguments;
assert cfg.services.thanos.sidecar.http-address == "127.0.0.1:10902";
assert cfg.services.thanos.store.http-address == "127.0.0.1:10904";
assert cfg.services.thanos.query.http-address == "127.0.0.1:10906";
assert cfg.services.thanos.compact.http-address == "127.0.0.1:10907";
assert cfg.services.thanos.sidecar.objstore.config-file == objectStorePath;
assert cfg.services.thanos.store.objstore.config-file == objectStorePath;
assert cfg.services.thanos.compact.objstore.config-file == objectStorePath;
assert objectStoreSecret.owner == "root";
assert objectStoreSecret.group == "thanos-objstore";
assert objectStoreSecret.mode == "0440";
assert builtins.hasAttr "thanos-objstore" cfg.users.groups;
assert lib.all (
  unit: unit.serviceConfig.SupplementaryGroups == [ "thanos-objstore" ]
) objectStoreReaders;
assert
  !(builtins.elem "thanos-objstore" (
    cfg.systemd.services.thanos-query.serviceConfig.SupplementaryGroups or [ ]
  ));
assert lib.all (unit: builtins.elem "multi-user.target" unit.wantedBy) thanosUnits;
assert lib.all (unit: (unit.serviceConfig.ExecStartPre or [ ]) == [ ]) thanosUnits;
assert lib.all (unit: (unit.serviceConfig.ExecStartPost or [ ]) == [ ]) thanosUnits;
assert lib.all (name: !(lib.hasInfix "thanos" name)) (
  builtins.attrNames cfg.system.activationScripts
);
assert !cfg.services.grafana.enable;
assert !(builtins.hasAttr "grafana" cfg.systemd.services);
assert !(builtins.hasAttr "pushover_api_token" cfg.sops.secrets);
assert !(builtins.hasAttr "pushover_user_key" cfg.sops.secrets);
pkgs.runCommand "debord-monitoring-test"
  {
    nativeBuildInputs = [
      alertmanagerCfg.package
      cfg.services.prometheus.package
      cfg.services.prometheus.package.cli
      pkgs.curl
      pkgs.jq
    ];
  }
  ''
    set -euo pipefail

    alertmanager_pid=""
    prometheus_pid=""
    cleanup() {
      for pid in "$prometheus_pid" "$alertmanager_pid"; do
        if [ -n "$pid" ]; then
          kill "$pid" 2>/dev/null || true
          wait "$pid" 2>/dev/null || true
        fi
      done
    }
    trap cleanup EXIT

    wait_for_url() {
      url="$1"
      attempts=0
      until curl --fail --silent --output /dev/null "$url"; do
        attempts=$((attempts + 1))
        if [ "$attempts" -ge 300 ]; then
          cat "$TMPDIR/alertmanager.log" "$TMPDIR/prometheus.log" >&2
          return 1
        fi
        sleep 0.1
      done
    }

    amtool check-config ${alertmanagerConfig}
    promtool check config ${pipelinePrometheusConfig}
    promtool check rules ${backupRules}
    substitute ${backupRulesTest} "$TMPDIR/debord-backup-alerts.test.yml" \
      --replace-fail '@BACKUP_RULES@' '${backupRules}'
    promtool test rules "$TMPDIR/debord-backup-alerts.test.yml"

    mkdir -p "$TMPDIR/alertmanager" "$TMPDIR/prometheus"
    alertmanager \
      --config.file=${alertmanagerConfig} \
      --web.listen-address=127.0.0.1:9093 \
      --cluster.listen-address= \
      --storage.path="$TMPDIR/alertmanager" \
      >"$TMPDIR/alertmanager.log" 2>&1 &
    alertmanager_pid=$!
    wait_for_url http://127.0.0.1:9093/-/ready

    prometheus \
      --config.file=${pipelinePrometheusConfig} \
      --web.listen-address=127.0.0.1:9090 \
      --storage.tsdb.path="$TMPDIR/prometheus" \
      --rules.alert.resend-delay=1s \
      >"$TMPDIR/prometheus.log" 2>&1 &
    prometheus_pid=$!
    wait_for_url http://127.0.0.1:9090/-/ready

    attempts=0
    until
      curl --fail --silent http://127.0.0.1:9090/api/v1/alerts >"$TMPDIR/prometheus-alerts.json"
      jq --exit-status \
        'any(.data.alerts[]?; .labels.alertname == "MonitoringPipelineTest" and .state == "firing")' \
        "$TMPDIR/prometheus-alerts.json" >/dev/null
    do
      attempts=$((attempts + 1))
      if [ "$attempts" -ge 300 ]; then
        cat "$TMPDIR/alertmanager.log" "$TMPDIR/prometheus.log" >&2
        exit 1
      fi
      sleep 0.1
    done

    attempts=0
    until
      curl --fail --silent http://127.0.0.1:9093/api/v2/alerts >"$TMPDIR/alertmanager-alerts.json"
      jq --exit-status \
        'any(.[]?; .labels.alertname == "MonitoringPipelineTest" and .status.state == "active")' \
        "$TMPDIR/alertmanager-alerts.json" >/dev/null
    do
      attempts=$((attempts + 1))
      if [ "$attempts" -ge 300 ]; then
        cat "$TMPDIR/alertmanager.log" "$TMPDIR/prometheus.log" >&2
        exit 1
      fi
      sleep 0.1
    done

    mkdir -p "$out"
    jq \
      '[.data.alerts[] | select(.labels.alertname == "MonitoringPipelineTest")]' \
      "$TMPDIR/prometheus-alerts.json" >"$out/prometheus-alerts.json"
    jq \
      '[.[] | select(.labels.alertname == "MonitoringPipelineTest")]' \
      "$TMPDIR/alertmanager-alerts.json" >"$out/alertmanager-alerts.json"
  ''
