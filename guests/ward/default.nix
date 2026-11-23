{
  config,
  lib,
  ...
}:
let
  inherit (config.site.net) svc;
  inherit (config.repo.secrets.global.domain) tailnet;
  address = lib.head svc.hosts4.ward;
  svcDns = lib.head svc.hosts4.${svc.dhcp.router};
  connectUrl = "http://op-connect.${tailnet}:8080";
  mootConnectToken = "/run/host-secrets/moot-op-connect-token";
in
{
  imports = [
    ../../modules/services/ts-egress.nix
    ../../modules/services/ts-broker.nix
    ../../modules/services/cliproxy-api.nix
    ./cliproxy-api.nix
  ];

  system.stateVersion = "26.05";

  modules.microvm-guest = {
    host = "debord";
    hostFQDN = "debord.${config.site.net.mgmt.domainName}";
    nixStoreBackend = "blk";
    mounts = [ "var/lib" ];
    hostSecrets = {
      enable = true;
      services = [
        "ts-egress"
        "ts-broker"
        "cliproxy-api"
      ];
    };
  };
  microvm = {
    vcpu = 2;
    mem = 2048;
  };

  modules.users.enable = lib.mkForce false;
  modules.services.sshd.enable = lib.mkForce false;
  security.sudo.enable = false;
  users.allowNoPasswordLogin = true;

  modules.services.ts-egress = {
    enable = true;
    hostname = "ward";
    tag = "tag:moot-egress";
    dnsUpstream = "${svcDns}:53";
    selfAddresses = [ address ];
    helper = {
      inherit address;
      interface = "svc";
      allowFrom = [ svc.subnet4 ];
    };
    ca.enable = true;
    onepassword = {
      url = connectUrl;
      tokenFile = mootConnectToken;
    };
    broker.socket = "/run/ts-broker/decide.sock";
  };

  modules.services.ts-broker = {
    enable = true;
    hostname = "moot-broker";
    onepassword = {
      url = connectUrl;
      tokenFile = mootConnectToken;
    };
  };

  modules.services.cliproxy-api.enable = true;
}
