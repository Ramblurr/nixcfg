_: {
  # ludwig sits on a remote LAN that is not part of our site network.
  networking.useNetworkd = true;
  networking.useDHCP = false;
  networking.nameservers = [
    "1.1.1.1"
    "9.9.9.9"
  ];
  # AdGuard Home binds 192.168.8.2 before networkd finishes configuring it.
  boot.kernel.sysctl."net.ipv4.ip_nonlocal_bind" = 1;

  systemd.network = {
    enable = true;
    # Any routable link will do: the fallback must not stall Samba and NFS.
    wait-online.anyInterface = true;
    # Intel X553 port 0 (FreeBSD ix0), the only cabled port.
    links."10-lan0" = {
      matchConfig.Path = "pci-0000:04:00.0";
      linkConfig.Name = "lan0";
    };
    networks."20-lan0" = {
      matchConfig.Name = "lan0";
      address = [
        "192.168.8.4/22"
        # Previously the adguardhome jail; LAN clients use it for DNS.
        "192.168.8.2/22"
      ];
      gateway = [ "192.168.8.1" ];
      networkConfig.IPv6AcceptRA = false;
      linkConfig.RequiredForOnline = "routable";
    };
    # Keeps the host at its known address if the rename above ever fails:
    # whichever port has a cable gets it. Ports without carrier stay
    # unconfigured; with anyInterface only one link has to come up.
    networks."90-fallback" = {
      matchConfig.Name = "en*";
      address = [ "192.168.8.4/22" ];
      gateway = [ "192.168.8.1" ];
      networkConfig.DHCP = "ipv4";
      linkConfig.RequiredForOnline = "routable";
    };
  };
}
