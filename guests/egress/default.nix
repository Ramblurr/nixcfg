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
in
{
  imports = [ ../../modules/services/ts-egress.nix ];

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
    onepassword.url = "http://op-connect.${tailnet}:8080";
  };
}
