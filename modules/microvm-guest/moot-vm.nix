{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.microvm-guest.mootVm;
  ipv4 = import ../../lib/ipv4.nix { inherit lib; };
  uplink = "uplink";
  mac = lib.my.generateMacAddress "1-moot-${config.networking.hostName}";
  proxy = "http://${cfg.network.hostAddress}:${toString cfg.network.proxyPort}";
  noProxy = "localhost,127.0.0.1,::1";
in
{
  options.modules.microvm-guest.mootVm = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      internal = true;
      description = "A Moot VM, whose only network is its tap to the tailnet node on its host. Set by mootWright and mootBuilder.";
    };

    network = {
      tapId = lib.mkOption {
        type = lib.types.strMatching "[a-zA-Z0-9-]{1,15}";
        description = "Tap interface name; must match the host's mootNodes record.";
      };
      hostAddress = lib.mkOption {
        inherit (ipv4) type;
        description = "The tailnet node's IPv4 address on the tap, used as the default gateway.";
      };
      guestAddress = lib.mkOption {
        inherit (ipv4) type;
        description = "Guest IPv4 address on the tap.";
      };
      prefixLength = lib.mkOption {
        type = lib.types.ints.between 1 32;
        default = 30;
        description = "Prefix length of the tap subnet.";
      };
      dnsResolver = lib.mkOption {
        type = lib.types.str;
        default = "1.1.1.1";
        description = "Public numeric DNS server for the guest; the node carries its queries through the exit node.";
      };
      proxyPort = lib.mkOption {
        type = lib.types.port;
        default = 1056;
        description = "Port of the node's HTTP proxy at hostAddress, which resolves tailnet names.";
      };
      ingressPort = lib.mkOption {
        type = lib.types.port;
        internal = true;
        description = "Guest port the node forwards its tailnet ingress to; set by the VM's role.";
      };
    };

    zvolSize = lib.mkOption {
      type = lib.types.ints.positive;
      description = "Size in MiB of the zvol holding the writable store.";
    };

    nix = {
      substituters = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Extra binary caches, fetched through the node's proxy.";
      };
      trustedPublicKeys = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Signing keys for the extra binary caches.";
      };
    };

    proxyEnv = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      readOnly = true;
      internal = true;
      default = {
        HTTPS_PROXY = proxy;
        HTTP_PROXY = proxy;
        https_proxy = proxy;
        http_proxy = proxy;
        NO_PROXY = noProxy;
        no_proxy = noProxy;
      };
      description = "Variables that send a process's HTTP through the node's proxy.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions =
      let
        inherit (cfg.network) hostAddress guestAddress prefixLength;
      in
      [
        {
          assertion = ipv4.isLink hostAddress guestAddress prefixLength;
          message = "mootVm needs hostAddress and guestAddress to be distinct host addresses of one /${toString prefixLength}.";
        }
      ];

    modules.microvm-guest = {
      autoNetSetup.enable = false;
      nixStoreBackend = "blk";
      writableStoreOverlay.zvolSize = cfg.zvolSize;
    };
    microvm = {
      hypervisor = "cloud-hypervisor";
      interfaces = [
        {
          type = "tap";
          id = cfg.network.tapId;
          inherit mac;
        }
      ];
    };

    networking.useDHCP = false;
    systemd.network = {
      enable = true;
      links."30-${uplink}" = {
        matchConfig.MACAddress = mac;
        linkConfig.Name = uplink;
      };
      networks."30-${uplink}" = {
        matchConfig.MACAddress = mac;
        address = [ "${cfg.network.guestAddress}/${toString cfg.network.prefixLength}" ];
        gateway = [ cfg.network.hostAddress ];
        networkConfig = {
          LinkLocalAddressing = "no";
          IPv6AcceptRA = false;
        };
      };
    };
    networking.firewall.interfaces.${uplink}.allowedTCPPorts = [ cfg.network.ingressPort ];
    services.resolved.enable = false;
    networking.resolvconf.enable = false;
    environment.etc."resolv.conf".text = ''
      nameserver ${cfg.network.dnsResolver}
    '';

    modules.users.enable = lib.mkForce false;
    modules.services.sshd.enable = lib.mkForce false;
    security.sudo.enable = false;
    users.allowNoPasswordLogin = true;
    services.openssh = {
      startWhenNeeded = true;
      openFirewall = false;
      hostKeys = [
        {
          path = "/var/lib/ssh/ssh_host_ed25519_key";
          type = "ed25519";
        }
      ];
      settings = {
        PermitRootLogin = "prohibit-password";
        PasswordAuthentication = false;
        KbdInteractiveAuthentication = false;
      };
    };
    systemd.sockets.sshd.enable = false;
    systemd.services."sshd-vsock@" = {
      wants = [ "sshd-keygen.service" ];
      after = [ "sshd-keygen.service" ];
    };

    security.pki.certificateFiles = [ ../../config/ts-egress-ca.pem ];

    nix.settings = {
      extra-substituters = lib.mkIf (cfg.nix.substituters != [ ]) cfg.nix.substituters;
      extra-trusted-public-keys = lib.mkIf (cfg.nix.trustedPublicKeys != [ ]) cfg.nix.trustedPublicKeys;
    };
    systemd.services.nix-daemon.environment = cfg.proxyEnv // {
      SSL_CERT_FILE = "/etc/ssl/certs/ca-certificates.crt";
    };
  };
}
