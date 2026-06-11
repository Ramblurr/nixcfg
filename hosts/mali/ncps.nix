{
  config,
  inputs,
  pkgs,
  lib,
  ...
}:

let
  inherit (config.repo.secrets.global) domain;
  cfg = config.services.ncps;
  hostName = "nix-cache.int.${domain.home}";
in
{
  sops.secrets.ncps_private_key = {
    owner = "ncps";
    group = "ncps";
  };
  # temporary workaround
  # ref: https://github.com/kalbasit/ncps/issues/1388
  # ref: https://github.com/kalbasit/ncps/issues/1329
  systemd.services.ncps.preStart = lib.mkForce ''
    ${lib.getExe cfg.package} migrate up --cache-database-url ${cfg.cache.databaseURL}
  '';
  services.ncps = {
    package = inputs.ncps.packages.${pkgs.stdenv.hostPlatform.system}.default;
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
      inherit hostName;
      maxSize = "50G";
      secretKeyPath = config.sops.secrets.ncps_private_key.path;
      lru = {
        scheduleTimeZone = "Europe/Berlin";
        schedule = "00 09 * * *"; # 9 AM daily
      };
      storage.local = "/mnt/fast/ncps";
      upstream = {
        urls = [
          "https://cache.nixos.org"
          "https://nix-community.cachix.org"
          "https://cache.numtide.com"
          config.repo.secrets.global.localAtticSubstituter
        ];
        publicKeys = [
          "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
          "niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g="
          config.repo.secrets.global.localAtticPublicKey
        ];
      };
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
      access_log /var/log/nginx/access-ncps.log;
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
    extra-substituters = [
      config.repo.secrets.global.nixCacheSubstituter
    ];
    extra-trusted-public-keys = [
      config.repo.secrets.global.nixCachePublicKey
    ];
  };
}
