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

  # plato is the LAN's subnet router. This node must not advertise the LAN:
  # as primary it receives tailnet traffic for its own LAN address but
  # replies through the site router, without SNAT, so the reply is lost.
  # Accepting routes would let plato's route take over LAN traffic.
  # `tailscale set` keeps prefs it is not given, so the empty value is needed.
  tailscaleFlags = [
    "--advertise-routes="
    "--accept-dns=false"
    "--accept-routes=false"
  ];

  # The former TrueNAS kern.hostid, recorded in the pool labels.
  hostId = "ac91e159";
  rootPool = "fast";
  # Mount point to dataset under <rootPool>/encrypted: the modules.boot.zfs
  # layout.
  datasets = {
    "/" = "local/root";
    "/nix" = "local/nix";
    "/home" = "local/home";
    "/persist" = "safe/persist";
  };
  # One ESP per root pool SSD. GRUB mirrors to every one; only the first is
  # required at boot.
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

  # State migrated from TrueNAS, outside the sops file.
  persist = {
    zfsKeys = "/persist/etc/zfs-keys";
    sshHostKeys = "/persist/etc/ssh";
    ups = "/persist/etc/ups";
    tailscaleAuthKey = "/persist/etc/tailscale/authkey";
  };

  # The TrueNAS periodic snapshot prefix, kept so existing snapshots age out
  # and Samba lists them.
  snapshotPrefix = "auto-";
}
