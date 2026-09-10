{ config, lib, ... }:
let
  name = config.networking.hostName;
  instance = import ../../config/immich-home.nix;
  address = net: builtins.head config.site.net.${net}.hosts4.${name};
  hostAddress = net: host: builtins.head config.site.net.${net}.hosts4.${host};
  networks = [
    "svc"
    "data"
  ];
in
{
  imports = [ ./immich.nix ];

  system.stateVersion = "26.05";

  modules.microvm-guest = {
    host = "dewey";
    hostFQDN = "dewey.${config.site.net.mgmt.domainName}";
    autoNetSetup.enable = false;
    mounts = [ "var/lib" ];
  };
  microvm = {
    vcpu = 2;
    mem = 4096;
    devices = lib.mkForce [ ];
    interfaces = map (net: {
      type = "macvtap";
      id = "imhome-${net}";
      mac = lib.my.generateMacAddress "${name}-${net}";
      macvtap = {
        link = "vlan-${net}";
        mode = "bridge";
      };
    }) networks;
  };
  # Keep the production guest's normal CPU mitigations, unlike dev guests.
  boot.kernelParams = lib.mkForce [ ];

  # This service guest has key-only root administration, not desktop users.
  modules.users.enable = lib.mkForce false;
  modules.services.sshd.enable = lib.mkForce false;
  networking = {
    useDHCP = false;
    nftables.enable = true;
    firewall.extraInputRules = ''
      iifname "svc" ip saddr { ${hostAddress "svc" "dewey"}, ${hostAddress "svc" "quine"} } tcp dport 22 accept
    '';
  };
  systemd.network = {
    enable = true;
    wait-online.anyInterface = false;
    networks = lib.genAttrs networks (net: {
      matchConfig.MACAddress = lib.my.generateMacAddress "${name}-${net}";
      address = [ "${address net}/${toString config.site.net.${net}.subnet4Len}" ];
      linkConfig.MTUBytes = config.site.net.${net}.mtu;
      networkConfig = {
        DHCP = "no";
        IPv6AcceptRA = false;
        LinkLocalAddressing = "no";
      };
      gateway = lib.optional (net == "svc") (hostAddress "svc" "addams");
      dns = lib.optional (net == "svc") (hostAddress "svc" "addams");
    });
    # Apply service-interface names before systemd's 99-default.link policy.
    links = lib.listToAttrs (
      map (net: {
        name = "10-${net}";
        value = {
          matchConfig.MACAddress = lib.my.generateMacAddress "${name}-${net}";
          linkConfig.Name = net;
        };
      }) networks
    );
  };

  services.openssh = {
    enable = true;
    openFirewall = lib.mkForce false;
    hostKeys = [
      {
        type = "ed25519";
        path = "/var/lib/sshd/ssh_host_ed25519_key";
      }
    ];
    settings = {
      PasswordAuthentication = lib.mkForce false;
      KbdInteractiveAuthentication = false;
      PermitRootLogin = "prohibit-password";
      PermitEmptyPasswords = lib.mkForce false;
    };
  };
  sops.age.sshKeyPaths = [ "/var/lib/sshd/ssh_host_ed25519_key" ];

  users.users.immich = {
    isSystemUser = true;
    inherit (instance) uid;
    group = "immich";
  };
  users.groups.immich.gid = instance.gid;
  fileSystems.${instance.mediaLocation} = {
    device = "${hostAddress "data" "mali"}:${instance.mediaExport}";
    fsType = "nfs";
    options = [
      "nfsvers=4.2"
      "hard"
      "_netdev"
      "noatime"
      "x-systemd.mount-timeout=30s"
    ];
  };
  systemd.tmpfiles.rules = [
    "d /var/lib/sshd 0700 root root -"
    "d /var/lib/immich-secrets 0700 root root -"
  ];
}
