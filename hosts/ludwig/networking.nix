_:
let
  site = import ./site.nix;
  inherit (site) lan;
  cidr = address: "${address}/${toString lan.prefixLength}";
in
{
  # ludwig sits on a remote LAN that is not part of our site network.
  networking.useNetworkd = true;
  networking.useDHCP = false;
  networking.nameservers = site.nameservers;
  # AdGuard Home binds its address before networkd finishes configuring it.
  boot.kernel.sysctl."net.ipv4.ip_nonlocal_bind" = 1;

  systemd.network = {
    # Any routable link will do: the fallback must not stall Samba and NFS.
    wait-online.anyInterface = true;
    # Intel X553 port 0 (FreeBSD ix0), the only cabled port.
    links."10-${lan.interface}" = {
      matchConfig.Path = "pci-0000:04:00.0";
      linkConfig.Name = lan.interface;
    };
    networks."20-${lan.interface}" = {
      matchConfig.Name = lan.interface;
      address = [
        (cidr lan.address)
        (cidr lan.dns)
      ];
      gateway = [ lan.gateway ];
      networkConfig.IPv6AcceptRA = false;
      linkConfig.RequiredForOnline = "routable";
    };
    # Keeps the host at its known address if the rename above ever fails:
    # whichever port has a cable gets it. Ports without carrier stay
    # unconfigured; with anyInterface only one link has to come up.
    networks."90-fallback" = {
      matchConfig.Name = "en*";
      address = [ (cidr lan.address) ];
      gateway = [ lan.gateway ];
      networkConfig.DHCP = "ipv4";
      linkConfig.RequiredForOnline = "routable";
    };
  };
}
