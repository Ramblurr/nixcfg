{
  config,
  lib,
  pkgs,
  ...
}:
let
  addamsLapiPort = 6001;
  addamsLapiUrl = "http://addams.${config.repo.secrets.global.domain.tailnet}:${toString addamsLapiPort}";
  crowdsecSecret = config.repo.secrets.local.crowdsec;
  siteNets = lib.attrByPath [ "site" "net" ] { } config;
  siteSubnet4s = map (netName: siteNets.${netName}.subnet4) (builtins.attrNames siteNets);
  trustedSourceCidrs =
    siteSubnet4s
    ++ [
      "127.0.0.0/8"
      "::1/128"
    ]
    ++ crowdsecSecret.trustedSourceCidrs;
  webCollections = [
    "crowdsecurity/caddy"
    "crowdsecurity/http-dos"
  ];
in
{
  users.users.crowdsec.extraGroups = [ config.services.caddy.group ];

  services.crowdsec = {
    enable = true;
    openFirewall = false;
    autoUpdateService = true;
    hub.collections = [
      "crowdsecurity/linux"
    ]
    ++ lib.optionals config.services.openssh.enable [ "crowdsecurity/sshd" ]
    ++ webCollections;
    localConfig.acquisitions =
      (lib.optionals config.services.openssh.enable [
        {
          source = "journalctl";
          journalctl_filter = [ "_SYSTEMD_UNIT=sshd.service" ];
          labels.type = "syslog";
        }
      ])
      ++ [
        {
          source = "journalctl";
          journalctl_filter = [ "_TRANSPORT=kernel" ];
          labels.type = "kernel";
        }
      ]
      ++ [
        {
          source = "file";
          filenames = [ "/var/log/caddy/access.log" ];
          labels.type = "caddy";
        }
      ];
    localConfig.parsers.s02Enrich = [
      {
        name = "local/whitelist-trusted-networks";
        description = "Whitelist site LAN and Tailscale source ranges.";
        whitelist = {
          reason = "trusted internal networks";
          cidr = trustedSourceCidrs;
        };
      }
    ];
    settings.general = {
      api.server.enable = false;
      cscli.output = "human";
    };
    settings.lapi.credentialsFile = config.sops.secrets."crowdsec/lapiCredentials".path;
  };

  # The module uses content-addressed filenames for local parsers, so remove
  # links left by older generations before tmpfiles recreates the current link.
  systemd.tmpfiles.settings."09-crowdsec-local-parser-cleanup" = {
    "/etc/crowdsec/parsers/s02-enrich/*-parsers-s02-enrich.yaml".r = { };
  };

  systemd.services = {
    crowdsec = {
      after = [ "tailscaled.service" ];
      wants = [ "tailscaled.service" ];
      serviceConfig = {
        Restart = "on-failure";
        RestartSec = lib.mkForce "5s";
        RestartSteps = 5;
        RestartMaxDelaySec = "60s";
      };
    };

    crowdsec-update-hub.serviceConfig.ExecStartPost = lib.mkForce [
      "+${pkgs.systemd}/bin/systemctl try-reload-or-restart crowdsec.service"
    ];

    crowdsec-firewall-bouncer = {
      # James's watcher and bouncer independently consume Addams LAPI.
      # The bouncer manages its own iptables chains; James has no firewall.service.
      after = lib.mkForce [
        "network-online.target"
        "tailscaled.service"
      ];
      wants = lib.mkForce [
        "network-online.target"
        "tailscaled.service"
      ];
      partOf = lib.mkForce [ ];
      serviceConfig = {
        Restart = "on-failure";
        RestartSec = "5s";
        RestartSteps = 5;
        RestartMaxDelaySec = "60s";
      };
    };
  };

  services.crowdsec-firewall-bouncer = {
    enable = true;
    registerBouncer.enable = false;
    secrets.apiKeyPath = config.sops.secrets."crowdsec/bouncerApiKey".path;
    settings = {
      api_url = addamsLapiUrl;
      mode = "iptables";
      log_mode = "stdout";
      update_frequency = "10s";
    };
  };

  environment.persistence."/persist".directories = [
    "/var/lib/crowdsec"
  ];

  sops.secrets."crowdsec/lapiCredentials" = {
    owner = "crowdsec";
    group = "crowdsec";
    mode = "0400";
    restartUnits = [ "crowdsec.service" ];
  };

  sops.secrets."crowdsec/bouncerApiKey" = {
    owner = "root";
    group = "root";
    mode = "0400";
    restartUnits = [ "crowdsec-firewall-bouncer.service" ];
  };
}
