authorizedKeys: _guestName: guestCfg:
{ lib, ... }:
let
  inherit (lib) mkForce;
in
{
  #node.name = guestCfg.nodeName;
  #node.type = guestCfg.backend;
  networking.firewall.allowedTCPPorts = [ 22 ];
  services.openssh = {
    enable = true;
    settings.PermitRootLogin = "yes";
    hostKeys = [
      {
        path = "/etc/ssh/ssh_host_ed25519_key";
        type = "ed25519";
      }
    ];
  };
  users.users.root = {
    openssh.authorizedKeys.keys = authorizedKeys;
  };

  nix = {
    settings.auto-optimise-store = mkForce false;
    optimise.automatic = mkForce false;
    gc.automatic = mkForce false;
  };

  systemd.network.enable = true;
  networking.useNetworkd = true;
  #systemd.network.networks."20-lan" = {
  #  matchConfig.Type = "ether";
  #  networkConfig = {
  #    DHCP = "yes";
  #    ##IPForward = "yes";
  #    #DNSSEC = "no";
  #    #Address = [ "10.9.4.201/22" ];
  #    #Gateway = "10.9.4.1";
  #    #DNS = [ "10.9.4.4" ];
  #    #IPv6AcceptRA = true;
  #    #DHCP = "no";
  #  };
  #};

  systemd.network.networks."10-${guestCfg.networking.mainLinkName}" = {
    matchConfig.Name = guestCfg.networking.mainLinkName;
    DHCP = "yes";
    # XXX: Do we really want this?
    dhcpV4Config.UseDNS = false;
    dhcpV6Config.UseDNS = false;
    ipv6AcceptRAConfig.UseDNS = false;
    networkConfig = {
      IPv6PrivacyExtensions = "yes";
      MulticastDNS = true;
      IPv6AcceptRA = true;
    };
    linkConfig.RequiredForOnline = "routable";
  };
}
