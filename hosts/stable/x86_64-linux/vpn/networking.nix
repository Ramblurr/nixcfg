{ config, lib, pkgs, ... }:
let vpn = config.repo.secrets.local;
in {
  sops.secrets."wireguard_private_key" = {
    mode = "400";
    owner = "root";
    group = "root";
  };
  networking = {
    nameservers = vpn.nameservers;
    defaultGateway = vpn.ipv4-gateway;
    defaultGateway6 = {
      address = "fe80::1";
      interface = "eth0";
    };
    dhcpcd.enable = false;
    usePredictableInterfaceNames = lib.mkForce false;
    interfaces = {
      eth0 = {
        ipv4.addresses = [{
          address = vpn.ipv4;
          prefixLength = 32;
        }];
        ipv6.addresses = [
          {
            address = vpn.ipv6-1;
            prefixLength = 64;
          }
          {
            address = vpn.ipv6-2;
            prefixLength = 64;
          }
        ];
        ipv4.routes = [{
          address = vpn.ipv4-route;
          prefixLength = 32;
        }];
        ipv6.routes = [{
          address = "fe80::1";
          prefixLength = 128;
        }];
      };
    };

    nat = {
      enable = true;
      internalInterfaces = [ "wg0" ];
      externalInterface = "eth0";
      forwardPorts = vpn.forwardPorts;
    };

    firewall = {
      enable = true;
      allowPing = true;
      logRefusedConnections = true;
      allowedTCPPorts = [ 53 ] ++ vpn.openPortsTCP;
      allowedUDPPorts = [ 53 51820 41641 ] ++ vpn.openPortsUDP;
    };

    wg-quick.interfaces = {
      wg0 = {
        address = [ "10.100.0.1/24" ];
        listenPort = 51820;
        postUp = ''
          ${pkgs.iptables}/bin/iptables -A FORWARD -i wg0 -j ACCEPT
          ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s 10.100.0.0/24 -o eth0 -j MASQUERADE
        '';
        preDown = ''
          ${pkgs.iptables}/bin/iptables -D FORWARD -i wg0 -j ACCEPT
          ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s 10.100.0.0/24 -o eth0 -j MASQUERADE
        '';
        privateKeyFile = config.sops.secrets.wireguard_private_key.path;
        peers = vpn.peers;
      };
    };
  };
  services.udev.extraRules = ''
    ATTR{address}=="${vpn.eth0}", NAME="eth0"

  '';
}
