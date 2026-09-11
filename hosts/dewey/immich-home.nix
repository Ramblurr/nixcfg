{ config, ... }:
let
  inherit (config.repo.secrets) home-ops;
  apiAddress = builtins.head config.site.net.svc.hosts4.immich-home;
  address = network: host: builtins.head config.site.net.${network}.hosts4.${host};
  connectAddress = address "prim" "dewey";
  workerAddress = address "prim" "peirce";
  instance = import ../../config/immich-home.nix;
  credentials = instance.machineLearning.credentials;
in
{
  # The private wrapper supplies the evaluated, secret-aware guest configuration.
  microvm.vms.immich-home = {
    autostart = true;
    restartIfChanged = true;
  };
  modules.services.onepassword-systemd-credentials.microvmSecrets.immich-home = {
    "immich-ml-ca.pem" = "${credentials.ca}/ca-certificate";
    "immich-ml-api-client.pem" = "${credentials.apiClient}/certificate";
    "immich-ml-api-client-key.pem" = "${credentials.apiClient}/private-key";
  };
  modules.services.caddy.routes.immich-home = {
    publicHost = "photos.${home-ops.homeDomain}";
    upstream = "${apiAddress}:2283";
    directWan = false;
    webSockets = true;
    requestBodyMaxSize = null;
  };
  networking.firewall.extraInputRules = ''
    iifname "prim" ip saddr ${workerAddress} ip daddr ${connectAddress} tcp dport 8080 accept
  '';
}
