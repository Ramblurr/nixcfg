{ config, pkgs, ... }:
let
  instance = import ../../config/immich-home.nix;
  address = network: host: builtins.head config.site.net.${network}.hosts4.${host};
  credentialDirectory = "/run/credentials/immich-ml-server-proxy.service";
  expiryCredentialDirectory = "/run/credentials/immich-ml-server-proxy-certificate-expiry.service";
in
{
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

  modules.services.immich-machine-learning = {
    enable = true;
    gpu = true;
    package = pkgs.callPackage ../../pkgs/immich-machine-learning-pascal.nix { };
  };

  modules.services.immich-ml-proxy = {
    enable = true;
    role = "server";
    listenAddress = address "prim" instance.machineLearning.host;
    serverName = "immich-ml.${instance.machineLearning.host}.${config.site.net.prim.domainName}";
    inherit (instance.machineLearning) port;
    allowedSourceAddresses = [
      (address "svc" "immich-home")
      (address "prim" instance.workerHost)
    ];
    loadCredentials = false;
    credentials = {
      ca = "${credentialDirectory}/ca.pem";
      certificate = "${credentialDirectory}/certificate.pem";
      privateKey = "${credentialDirectory}/private-key.pem";
    };
    authorizedClientCertificates = {
      "api-client.pem" = "${credentialDirectory}/api-client.pem";
      "worker-client.pem" = "${credentialDirectory}/worker-client.pem";
    };
    loadMonitoringCredentials = false;
    monitoredCertificates = {
      "api-client.pem" = "${expiryCredentialDirectory}/api-client.pem";
      "server.pem" = "${expiryCredentialDirectory}/server.pem";
      "worker-client.pem" = "${expiryCredentialDirectory}/worker-client.pem";
    };
  };

  systemd.services.immich-ml-server-proxy = {
    requires = [ "immich-machine-learning.service" ];
    after = [ "immich-machine-learning.service" ];
  };
}
