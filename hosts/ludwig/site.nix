# Facts shared by the ludwig host's modules.
rec {
  lan = {
    interface = "lan0";
    address = "192.168.8.4";
    prefixLength = 22;
    subnet = "192.168.8.0/${toString lan.prefixLength}";
    gateway = "192.168.8.1";
    # AdGuard Home; LAN clients use it for DNS.
    dns = "192.168.8.2";
  };
  nameservers = [
    "1.1.1.1"
    "9.9.9.9"
  ];

  # This node must not advertise the LAN: as primary it receives tailnet
  # traffic for its own LAN address but replies through the site router,
  # without SNAT, so the reply is lost.
  # `tailscale set` keeps prefs it is not given, so the empty value is needed.
  tailscaleFlags = [
    "--advertise-routes="
    "--accept-dns=false"
    "--accept-routes=false"
  ];

  # The former TrueNAS kern.hostid, recorded in the pool labels.
  hostId = "ac91e159";
  rootPool = "fast";
  # Mount point to dataset under <rootPool>/encrypted.
  datasets = {
    "/" = "local/root";
    "/nix" = "local/nix";
    "/home" = "local/home";
    "/persist" = "safe/persist";
  };
  esps = [
    {
      mountPoint = "/boot";
      label = "ludwig-esp0";
    }
    {
      mountPoint = "/boot-fallback";
      label = "ludwig-esp1";
    }
  ];

  persist = {
    zfsKeys = "/persist/etc/zfs-keys";
    sshHostKeys = "/persist/etc/ssh";
    ups = "/persist/etc/ups";
    tailscaleAuthKey = "/persist/etc/tailscale/authkey";
  };

  # Existing snapshots carry this prefix; pruning and Samba match on it.
  snapshotPrefix = "auto-";
}
