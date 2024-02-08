{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  k3s-test = builtins.fromJSON (builtins.readFile ../../../../secrets/k3s-test.secrets);
  current-server = k3s-test.servers.${config.networking.hostName};
in {
  modules.server.k3s-server = {
    enable = true;
    endpointVip = k3s-test.endpointVip;
    nodeIp = current-server.nodeIp;
  };

  environment.etc."k3s-token" = {
    user = "root";
    mode = "0600";
    text = k3s-test.k3s-token;
  };
}
