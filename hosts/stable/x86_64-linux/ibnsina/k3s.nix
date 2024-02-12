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
  modules.profiles.k3s-node = {
    enable = true;
    server.enable = true;
    clusterSettings = k3s-main;
    nodeSettings = current-server;
  };
}
