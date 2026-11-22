{
  config,
  lib,
  ...
}:
let
  inherit (config.site.net) svc;
  inherit (config.repo.secrets.global.domain) tailnet;
  address = lib.head svc.hosts4.egress;
  svcDns = lib.head svc.hosts4.${svc.dhcp.router};
  connectUrl = "http://op-connect.${tailnet}:8080";
in
{
  imports = [
    ../../modules/services/ts-egress.nix
    ../../modules/services/ts-broker.nix
  ];

  system.stateVersion = "26.05";

  modules.microvm-guest = {
    host = "quine";
    hostFQDN = "quine.${config.site.net.mgmt.domainName}";
    bootstrapSops.enable = true;
    nixStoreBackend = "blk";
    mounts = [ "var/lib" ];
    # quine's modules.services.onepassword-systemd-credentials.microvmSecrets.egress
    # fills /run/host-secrets, the ts-egress module's default credential sources.
    hostSecrets = {
      enable = true;
      services = [
        "ts-egress"
        "ts-broker"
        "tailscaled-autoconnect"
      ];
    };
  };
  microvm = {
    # bootstrapSops passes the host key as a credential, which
    # cloud-hypervisor does not support.
    hypervisor = "qemu";
    vcpu = 2;
    mem = 1536;
  };

  modules.users.enable = lib.mkForce false;
  modules.services.sshd.enable = lib.mkForce false;
  security.sudo.enable = false;

  # The egress resolves Connect's tailnet name without taking Tailscale DNS.
  services.resolved.dnsDelegates.tailnet.Delegate = {
    DNS = "100.100.100.100";
    Domains = [ "~${tailnet}" ];
  };

  modules.services.ts-egress = {
    enable = true;
    dnsUpstream = "${svcDns}:53";
    selfAddresses = [ address ];
    helper = {
      inherit address;
      interface = "svc";
      allowFrom = [ svc.subnet4 ];
    };
    ca.enable = true;
    # modules.services.onepassword-connect.tailnetUrl, which guests cannot
    # import; it stays plain HTTP until every replica's Tailscale is 1.106+.
    onepassword.url = connectUrl;
    broker.socket = "/run/ts-broker/decide.sock";
  };

  # The prototype's broker reads its catalog with the egress's Connect token.
  # A real deployment gives it its own token, the module's default source.
  modules.services.ts-broker = {
    enable = true;
    onepassword = {
      url = connectUrl;
      tokenFile = config.modules.services.ts-egress.onepassword.tokenFile;
    };
  };

  # tailscaled answers the node's own 443 in its netstack, before the TPROXY
  # rules see the packet, and proxies to the broker's loopback port. The
  # serve config persists in tailscaled's state; setting it again is a no-op.
  systemd.services.ts-broker-serve = {
    description = "Serve the ts-broker UI on the tailnet";
    wantedBy = [ "multi-user.target" ];
    requires = [ "tailscaled.service" ];
    wants = [ "ts-broker.service" ];
    after = [
      "tailscaled.service"
      "tailscaled-autoconnect.service"
      "tailscaled-set.service"
      "ts-broker.service"
    ];
    startLimitIntervalSec = 0;
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "${lib.getExe config.services.tailscale.package} serve --bg --yes --https=443 http://127.0.0.1:${toString config.modules.services.ts-broker.port}";
      Restart = "on-failure";
      RestartSec = 5;
    };
  };
}
