{
  config,
  lib,
  pkgs,
  ...
}: {
  services.nfs.server = {
    enable = true;
    exports = "";
    statdPort = 4000;
    lockdPort = 4001;
    mountdPort = 4002;
  };
  networking.firewall.allowedTCPPorts = [111 2049 4000 4001 4002 20048];
  networking.firewall.allowedUDPPorts = [111 2049 4000 4001 4002 20048];

  sops.secrets."nfsExports" = {
    sopsFile = ./secrets.sops.yaml;
    restartUnits = [];
  };

  environment.etc."exports".source = lib.mkForce config.sops.secrets.nfsExports.path;
}
