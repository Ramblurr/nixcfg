{
  config,
  lib,
  pkgs,
  ...
}:
let
  instance = import ../../config/immich-home.nix;
  credentials = instance.machineLearning.credentials;
  address = network: host: builtins.head config.site.net.${network}.hosts4.${host};
  credentialDirectory = "/run/credentials/immich-ml-server-proxy.service";
  expiryCredentialDirectory = "/run/credentials/immich-ml-server-proxy-certificate-expiry.service";
in
{
  imports = [ ../../config/immich-worker.nix ];

  hardware.graphics.enable = true;
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia = {
    open = false;
    # The GTX 1070 Ti uses Pascal. R580 is the last NVIDIA driver series
    # that supports this GPU. Newer driver series cannot be used.
    package = config.boot.kernelPackages.nvidiaPackages.legacy_580;
    nvidiaSettings = false;
  };
  boot.kernelModules = [ "nvidia_uvm" ];
  nix.settings = {
    extra-substituters = [ "https://cache.flox.dev" ];
    extra-trusted-public-keys = [ "flox-cache-public-1:7F4OyH7ZCnFhcze3fJdfyXYLQw/aV7GEed86nQ7IsOs=" ];
  };

  modules.services.onepassword-systemd-credentials = {
    enable = true;
    consumers.immich-ml-server-proxy = {
      "ca.pem" = "${credentials.ca}/ca-certificate";
      "certificate.pem" = "${credentials.server}/certificate";
      "private-key.pem" = "${credentials.server}/private-key";
      "api-client.pem" = "${credentials.apiClient}/certificate";
    };
    consumers.immich-ml-server-proxy-certificate-expiry = {
      "api-client.pem" = "${credentials.apiClient}/certificate";
      "server.pem" = "${credentials.server}/certificate";
    };
  };

  modules.services.immich-machine-learning = {
    enable = true;
    gpu = true;
    port = instance.machineLearning.rawPort;
    package = pkgs.callPackage ../../pkgs/immich-machine-learning-pascal.nix { };
  };

  modules.services.immich-ml-proxy = {
    enable = true;
    role = "server";
    listenAddress = address "prim" instance.machineLearning.host;
    serverName = "immich-ml.${instance.machineLearning.host}.${config.site.net.prim.domainName}";
    inherit (instance.machineLearning) port;
    allowedSourceAddresses = [ (address "svc" "immich-home") ];
    loadCredentials = false;
    credentials = {
      ca = "${credentialDirectory}/ca.pem";
      certificate = "${credentialDirectory}/certificate.pem";
      privateKey = "${credentialDirectory}/private-key.pem";
    };
    authorizedClientCertificates = {
      "api-client.pem" = "${credentialDirectory}/api-client.pem";
    };
    loadMonitoringCredentials = false;
    monitoredCertificates = {
      "api-client.pem" = "${expiryCredentialDirectory}/api-client.pem";
      "server.pem" = "${expiryCredentialDirectory}/server.pem";
    };
  };

  site.gatus.heartbeats.immich-ml-certificate-expiry =
    lib.mkIf config.site.gatus.heartbeatToken.available
      {
        service = "immich-ml-server-proxy-certificate-expiry";
        name = "Immich ML Certificate Expiry";
        group = config.site.gatus.groups.infrastructure;
        interval = "36h";
      };

  assertions = [
    {
      assertion = instance.workerHost == config.networking.hostName;
      message = "Peirce must remain the selected Immich background worker host.";
    }
    {
      assertion =
        config.modules.services.immich-worker.enable
        && config.services.immich.enable
        && !config.services.immich.machine-learning.enable
        &&
          config.services.immich.environment.IMMICH_MACHINE_LEARNING_URL
          == "http://127.0.0.1:${toString instance.machineLearning.rawPort}";
      message = "Peirce must run the background worker against its standalone loopback ML service.";
    }
  ];

  systemd.services.immich-ml-server-proxy = {
    requires = [ "immich-machine-learning.service" ];
    after = [ "immich-machine-learning.service" ];
  };
}
