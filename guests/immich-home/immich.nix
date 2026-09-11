{
  config,
  lib,
  pkgs,
  ...
}:
let
  instance = import ../../config/immich-home.nix;
  address = host: builtins.head config.site.net.svc.hosts4.${host};
  apiAddress = address config.networking.hostName;
  workerAddress = address instance.workerHost;
  machineLearningAddress = builtins.head config.site.net.prim.hosts4.${instance.machineLearning.host};
  machineLearningServerName = "immich-ml.${instance.machineLearning.host}.${config.site.net.prim.domainName}";
  hostSecrets = "/run/host-secrets";
  secretsFile = "${instance.secretsDirectory}/environment";
in
{
  imports = [
    ../../modules/services/immich-common.nix
    ../../modules/services/immich-ml-proxy.nix
  ];

  modules.services.immich-ml-proxy = {
    enable = true;
    role = "client";
    port = instance.machineLearning.localProxyPort;
    upstreamAddress = machineLearningAddress;
    upstreamPort = instance.machineLearning.port;
    serverName = machineLearningServerName;
    allowedUser = config.services.immich.user;
    credentials = {
      ca = "${hostSecrets}/immich-ml-ca.pem";
      certificate = "${hostSecrets}/immich-ml-api-client.pem";
      privateKey = "${hostSecrets}/immich-ml-api-client-key.pem";
    };
  };

  services.immich = {
    enable = true;
    host = apiAddress;
    inherit (instance) mediaLocation;
    inherit secretsFile;
    accelerationDevices = [ ];
    machine-learning.enable = false;
    environment = {
      IMMICH_WORKERS_INCLUDE = "api";
      # Initial default only; the database-backed admin UI remains authoritative.
      IMMICH_MACHINE_LEARNING_URL = lib.mkForce "http://127.0.0.1:${toString instance.machineLearning.localProxyPort}";
    };
    redis = {
      host = apiAddress;
      port = 6379;
    };
  };

  services.postgresql = {
    package = pkgs.postgresql_18;
    enableTCPIP = true;
    settings.listen_addresses = lib.mkForce apiAddress;
    authentication = lib.mkForce ''
      local all postgres peer
      local immich immich peer
      host immich immich ${workerAddress}/32 scram-sha-256
    '';
  };
  systemd.services.postgresql-setup.serviceConfig = {
    EnvironmentFile = secretsFile;
    # psql reads the password from its environment, never from argv or the store.
    ExecStartPost = lib.mkAfter [
      (pkgs.writeShellScript "immich-database-password" ''
        set -eu
        test -n "$DB_PASSWORD"
        ${lib.getExe' config.services.postgresql.package "psql"} -v ON_ERROR_STOP=1 -d postgres <<'SQL'
        \getenv immich_password DB_PASSWORD
        ALTER ROLE immich PASSWORD :'immich_password';
        \unset immich_password
        SQL
      '')
    ];
  };
  services.redis.servers.immich = {
    requirePassFile = "${instance.secretsDirectory}/redis-password";
    settings = {
      appendonly = "yes";
      appendfsync = "everysec";
    };
  };
  systemd.services.immich-server = {
    requires = [
      "redis-immich.service"
    ]
    ++ lib.optionals config.modules.services.immich-ml-proxy.enable [
      "immich-ml-client-proxy.service"
    ];
    after = [
      "redis-immich.service"
    ]
    ++ lib.optionals config.modules.services.immich-ml-proxy.enable [
      "immich-ml-client-proxy.service"
    ];
  };
  systemd.tmpfiles.rules = [ "d ${instance.secretsDirectory} 0700 root root -" ];

  networking.firewall.extraInputRules = ''
    ip saddr ${address "dewey"} ip daddr ${apiAddress} tcp dport 2283 accept
    ip saddr ${workerAddress} ip daddr ${apiAddress} tcp dport { 5432, 6379 } accept
  '';
}
