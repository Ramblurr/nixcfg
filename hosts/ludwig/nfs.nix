{
  config,
  ...
}:
let
  server = config.services.nfs.server;
  # rpcbind, nfsd and the pinned NFSv3 side daemons.
  ports = [
    111
    2049
    server.statdPort
    server.lockdPort
    server.mountdPort
  ];
in
{
  services.nfs.server = {
    enable = true;
    statdPort = 4000;
    lockdPort = 4001;
    mountdPort = 4002;
    exports = config.repo.secrets.local.ludwig.nfsExports;
  };
  networking.firewall.allowedTCPPorts = ports;
  networking.firewall.allowedUDPPorts = ports;
}
