{
  config,
  ...
}:
let
  inherit (config.repo.secrets.global) domain;

  smartPort = toString config.services.prometheus.exporters.smartctl.port;
  zfsPort = toString config.services.prometheus.exporters.zfs.port;
  nutPort = toString config.services.prometheus.exporters.nut.port;
  ipmiPort = toString config.services.prometheus.exporters.ipmi.port;
  nodePort = toString config.services.prometheus.exporters.node.port;
  zreplPort = toString config.repo.secrets.home-ops.ports.zrepl-metrics;

  mkStaticConfig = port: host: { targets = [ "${host}.mgmt.${domain.home}:${port}" ]; };
  mkStaticConfigs = port: hosts: map (mkStaticConfig port) hosts;
  mkLocalScrape = jobName: target: {
    job_name = jobName;
    static_configs = [ { targets = [ target ]; } ];
  };
in
{
  modules.telemetry.prometheus.enable = true;
  modules.telemetry.thanos.enable = true;
  services.prometheus.ruleFiles = [ ./backup-alerts.rules.yml ];
  services.prometheus.alertmanagers = [
    {
      static_configs = [ { targets = [ "127.0.0.1:9093" ]; } ];
    }
  ];

  modules.services.caddy.edge.certificateHosts = map (host: "${host}.${domain.home}") [
    "alertmanager"
    "prom"
    "thanos"
  ];

  site.gatus.endpoints = [
    {
      name = "Alertmanager";
      group = config.site.gatus.groups.infrastructure;
      url = "https://alertmanager.${domain.home}/-/healthy";
    }
    {
      name = "Prometheus";
      group = config.site.gatus.groups.infrastructure;
      url = "https://prom.${domain.home}/-/healthy";
    }
    {
      name = "Thanos Query";
      group = config.site.gatus.groups.infrastructure;
      url = "https://thanos.${domain.home}/-/healthy";
    }
  ];

  modules.services.caddy.routes = {
    prom = {
      publicHost = "prom.${domain.home}";
      upstream = "http://127.0.0.1:${toString config.services.prometheus.port}";
    };
    thanos = {
      publicHost = "thanos.${domain.home}";
      upstream = "http://${config.services.thanos.query.http-address}";
    };
    alertmanager = {
      publicHost = "alertmanager.${domain.home}";
      upstream = "http://127.0.0.1:${toString config.services.prometheus.alertmanager.port}";
    };
  };

  services.prometheus.alertmanager = {
    enable = true;
    checkConfig = true;
    listenAddress = "127.0.0.1";
    openFirewall = false;
    # Standalone mode needs no gossip listener.
    extraFlags = [ "--cluster.listen-address=" ];

    # This named receiver intentionally records alert state without notifying anyone.
    # Grouping and repeat timings keep their defaults because no integration consumes them.
    configuration = {
      route.receiver = "null";
      receivers = [ { name = "null"; } ];
    };
  };

  services.prometheus.scrapeConfigs = [
    {
      job_name = "node";
      static_configs = mkStaticConfigs nodePort [
        "dewey"
        "debord"
        "mali"
        "addams"
      ];

    }
    {
      job_name = "smartd";
      static_configs = mkStaticConfigs smartPort [
        "dewey"
        "debord"
        "mali"
        "addams"
      ];
    }
    {
      job_name = "zfs";
      static_configs = mkStaticConfigs zfsPort [
        "dewey"
        "debord"
        "mali"
        "addams"
      ];
    }
    {
      job_name = "nut";
      static_configs = mkStaticConfigs nutPort [ "mali" ];
    }
    {
      job_name = "ipmi";
      static_configs = mkStaticConfigs ipmiPort [ "mali" ];
    }
  ]
  ++ (map
    (host: {
      job_name = "zrepl_${host}";
      static_configs = mkStaticConfigs zreplPort [
        host
      ];
    })
    [
      "dewey"
      "debord"
      "mali"
    ]
  )
  ++ [
    (mkLocalScrape "prometheus" "127.0.0.1:${toString config.services.prometheus.port}")
    (mkLocalScrape "alertmanager" "127.0.0.1:${toString config.services.prometheus.alertmanager.port}")
    (mkLocalScrape "thanos-sidecar" config.services.thanos.sidecar.http-address)
    (mkLocalScrape "thanos-store" config.services.thanos.store.http-address)
    (mkLocalScrape "thanos-query" config.services.thanos.query.http-address)
    (mkLocalScrape "thanos-compact" config.services.thanos.compact.http-address)
  ];
}
