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
in
{
  modules.telemetry.prometheus.enable = true;
  modules.telemetry.thanos.enable = true;
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
  );
}
