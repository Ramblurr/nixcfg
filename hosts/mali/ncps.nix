{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (config.repo.secrets.global) domain;
  hostName = "nix-cache.int.${domain.home}";
in
{
  sops.secrets.ncps_private_key = {
    owner = "ncps";
    group = "ncps";
  };
  services.ncps = {
    enable = true;
    logLevel = "info";
    prometheus = {
      enable = false;
    };
    server = {
      addr = "127.0.0.1:${toString config.repo.secrets.home-ops.ports.ncps}";
    };
    cache = {
      allowDeleteVerb = false;
      allowPutVerb = false;
      hostName = hostName;
      maxSize = "50G";
      secretKeyPath = config.sops.secrets.ncps_private_key.path;
      lru = {
        scheduleTimeZone = "Europe/Berlin";
        schedule = "00 09 * * *"; # 9 AM daily
      };
      dataPath = "/mnt/fast/ncps";
    };
    upstream = {
      caches = [
        "https://cache.nixos.org"
        "https://nix-community.cachix.org"
        config.repo.secrets.global.localAtticSubstituter
      ];
      publicKeys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        config.repo.secrets.global.localAtticPublicKey
      ];
    };
  };
  systemd.tmpfiles.rules = [
    "d /var/lib/ncps 770 ncps ncps"
    "z /var/lib/ncps 770 ncps ncps"
    "z /mnt/fast/ncps 770 ncps ncps"
  ];
  environment.persistence."/persist".directories = [ "/var/lib/ncps" ];
  services.nginx.virtualHosts.${hostName} = {
    useACMEHost = hostName;
    forceSSL = true;
    http3 = false;
    http2 = false;
    kTLS = true;
    extraConfig = ''
      client_max_body_size 0;
    '';
    locations."/" = {
      proxyPass = "http://${config.services.ncps.server.addr}";
      recommendedProxySettings = true;
    };
  };
  security.acme.certs.${hostName} = {
    domain = hostName;
    group = "nginx";
  };

  #services.prometheus.scrapeConfigs = [
  #  {
  #    job_name = "ncps";
  #    static_configs = [ { targets = [ "${config.services.ncps.server.addr}" ]; } ];
  #  }
  #];

  nix.settings = {
    substituters = [
      config.repo.secrets.global.nixCacheSubstituter
    ];
    trusted-public-keys = [
      config.repo.secrets.global.nixCachePublicKey
    ];
  };
}
