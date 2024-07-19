authorizedKeys: _guestName: guestCfg:
{ lib, pkgs, ... }:
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

  environment.systemPackages = with pkgs; [ kitty.terminfo ];

  systemd.network.enable = true;
  networking.useNetworkd = true;
  systemd.network.networks."10-${guestCfg.networking.mainLinkName}" = {
    matchConfig.Name = guestCfg.networking.mainLinkName;
    #matchConfig.Type = "ether";
    DHCP = "no";
    # XXX: Do we really want this?
    #dhcpV4Config.UseDNS = false;
    #dhcpV6Config.UseDNS = false;
    #ipv6AcceptRAConfig.UseDNS = false;
    networkConfig = {
      Address = [ guestCfg.networking.address ];
      Gateway = guestCfg.networking.gateway;
      DNS = guestCfg.networking.dns;
      #IPv6PrivacyExtensions = "yes";
      #MulticastDNS = true;
      #IPv6AcceptRA = true;
    };
    linkConfig.RequiredForOnline = "routable";
  };
}
