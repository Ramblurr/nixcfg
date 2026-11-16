# Facts shared by the ludwig host, its installer and their scripts.
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

  # The remote LAN stays reachable through this node, and no peer route may
  # take over LAN traffic.
  tailscaleFlags = [
    "--advertise-routes=${lan.subnet}"
    "--accept-dns=false"
    "--accept-routes=false"
  ];

  # TrueNAS kern.hostid, recorded in the pool labels.
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
  # One ESP per root pool SSD, in the order of install.sh's ESP_UUIDS. GRUB
  # mirrors to every one; only the first is required at boot.
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
  truenasEspUuid = "d4fb2736-d81c-11ec-bff8-d05099fb0f82";
  # Directory on the TrueNAS ESP that truenas-stage.sh writes.
  bundleDir = "ludwig-installer";

  # Where install.sh seeds the migrated state and the host reads it.
  persist = {
    zfsKeys = "/persist/etc/zfs-keys";
    sshHostKeys = "/persist/etc/ssh";
    ups = "/persist/etc/ups";
    smbpasswd = "/persist/etc/ludwig-migration/smbpasswd";
    tailscaleAuthKey = "/persist/etc/tailscale/authkey";
  };

  # The TrueNAS periodic snapshot prefix, kept so existing snapshots age out
  # and Samba lists them.
  snapshotPrefix = "auto-";
}
