{
  lib,
  pkgs,
  ...
}:

{

  networking.firewall.logRefusedConnections = lib.mkForce false;
  modules.vpn.tailscale.enable = true;
  modules.vpn.tailscale.useRoutingFeatures = "both";
  boot.kernel.sysctl = {
    "net.ipv4.ip_forward" = true;
    "net.ipv4.conf.all.forwarding" = true;
    "net.ipv6.conf.all.forwarding" = true;
  };
  networking.firewall.allowedUDPPorts = [
    4214
  ];
  networking.firewall.allowedTCPPorts = [
    10700 # wyoming-satellite
    4214
  ];
  systemd.network.networks."30-prim" = {
    routingPolicyRules = [
      {
        FirewallMark = 66; # 0x42 in decimal
        Table = 142;
        Priority = 100;
      }
    ];
    routes = [
      {
        Table = 142;
        Destination = "10.9.4.0/22";
        Gateway = "10.9.4.1";
      }
      {
        Table = 142;
        Destination = "0.0.0.0/0";
        Gateway = "10.9.4.1";
      }
    ];
  };
  services.networkd-dispatcher = {
    enable = true;
    rules = {
      # Add tailscale route to custom routing table 142
      "50-tailscale-routing-table" = {
        onState = [ "routable" ];
        script = ''
          #!${pkgs.runtimeShell}
          if [[ "$IFACE" == "tailscale0" ]]; then
            # Add route for remote network via tailscale to table 142
            ${pkgs.iproute2}/bin/ip route add 192.168.8.0/22 dev tailscale0 table 142 2>/dev/null || true
          fi
        '';
      };

      # Add nftables rules for connection tracking
      "50-tailscale-marking-rules" = {
        onState = [ "routable" ];
        script = ''
          #!${pkgs.runtimeShell}
          if [[ "$IFACE" == "tailscale0" ]]; then
            # Create mangle table and add marking rules
            ${pkgs.nftables}/bin/nft add table ip mangle 2>/dev/null || true
            ${pkgs.nftables}/bin/nft add chain ip mangle PREROUTING '{ type filter hook prerouting priority mangle ; }' 2>/dev/null || true
            ${pkgs.nftables}/bin/nft add rule ip mangle PREROUTING iifname tailscale0 ip daddr 10.9.4.0/22 meta mark set 0x42 2>/dev/null || true

            # Insert connection marking rule before ts-forward jump
            ${pkgs.nftables}/bin/nft insert rule ip filter FORWARD iifname prim ip saddr 10.9.4.0/22 ip daddr 192.168.8.0/22 ct state new ct mark set 0x42 2>/dev/null || true
          fi
        '';
      };
    };
  };
}
