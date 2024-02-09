{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  k3s-main = builtins.fromJSON (builtins.readFile ../../../../secrets/k3s-main.secrets);
  current-server = k3s-main.servers.${config.networking.hostName};
in {
  modules.server.k3s-server = {
    enable = true;
    endpointVip = k3s-main.endpointVip;
    nodeIp = current-server.nodeIp;
  };

  environment.etc."k3s-token" = {
    user = "root";
    mode = "0600";
    text = k3s-main.k3s-token;
  };
}
