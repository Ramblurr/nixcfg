{
  config,
  lib,
  ...
}:
let
  addamsLapiPort = 6001;
  addamsLapiUrl = "http://addams.${config.repo.secrets.global.domain.tailnet}:${toString addamsLapiPort}";
in
{
  users.users.crowdsec.extraGroups = lib.optionals config.services.nginx.enable [
    config.services.nginx.group
  ];

  services.crowdsec = {
    enable = true;
    openFirewall = false;
    autoUpdateService = true;
    hub.collections = [
      "crowdsecurity/linux"
    ]
    ++ lib.optionals config.services.openssh.enable [ "crowdsecurity/sshd" ]
    ++ lib.optionals config.services.nginx.enable [
      "crowdsecurity/nginx"
      "crowdsecurity/http-dos"
    ];
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
      ++ lib.optionals config.services.nginx.enable [
        {
          source = "file";
          filenames = [ "/var/log/nginx/crowdsec.log" ];
          labels.type = "nginx";
        }
      ];
    settings.general = {
      api.server.enable = false;
      cscli.output = "human";
    };
    settings.lapi.credentialsFile = config.sops.secrets."crowdsec/lapiCredentials".path;
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
