{
  lib,
  ...
}:

{

  networking.firewall.logRefusedConnections = lib.mkForce false;
  modules.vpn.tailscale.enable = true;
  modules.vpn.tailscale.useRoutingFeatures = "both";
  services.tailscale.extraSetFlags = [
    "--advertise-exit-node=true"
    "--snat-subnet-routes=true"
    "--ssh=true"
  ];
  # The tunnel MTU is below the LAN's. Clamp the MSS of TCP forwarded into it,
  # so connections through the subnet routes do not depend on path MTU
  # discovery (ICMP) getting through. plato carries the same rule.
  networking.nftables.tables.mss-clamp = {
    family = "inet";
    content = ''
      chain forward {
        type filter hook forward priority mangle; policy accept;
        oifname "tailscale0" tcp flags syn tcp option maxseg size set rt mtu
      }
    '';
  };
  boot.kernel.sysctl = {
    "net.ipv4.ip_forward" = true;
    "net.ipv4.conf.all.forwarding" = true;
    "net.ipv6.conf.all.forwarding" = true;
  };
  networking.firewall.allowedUDPPorts = [
    4214
  ];
  networking.firewall.allowedTCPPorts = [
    4214
  ];
  systemd.network.config.networkConfig = {
    IPv4Forwarding = true;
    IPv6Forwarding = true;
  };
}
