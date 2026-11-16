{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (config.repo.secrets.local) ludwig;
  smbpasswd = "/persist/etc/ludwig-migration/smbpasswd";
  shareDefaults = {
    browseable = "yes";
    "read only" = "no";
    "guest ok" = "no";
    "create mask" = "0664";
    # Previous Versions from the daily auto-* snapshots (TrueNAS shadowcopy).
    "vfs objects" = "shadow_copy2 streams_xattr";
    "shadow:snapdir" = ".zfs/snapshot";
    "shadow:sort" = "desc";
    "shadow:format" = "auto-%Y-%m-%d_%H-%M";
    "shadow:localtime" = "yes";
  };
in
{
  services.samba = {
    enable = true;
    openFirewall = true;
    nmbd.enable = true;
    winbindd.enable = false;
    settings = {
      global = {
        # Clients already know this server by its TrueNAS name.
        "netbios name" = "truenas";
        workgroup = "WORKGROUP";
        "server string" = "ludwig";
        "server role" = "standalone";
        security = "user";
        "server min protocol" = "SMB2_02";
        "restrict anonymous" = "2";
        "map to guest" = "Never";
        "unix charset" = "UTF-8";
        "ea support" = "no";
        "dos filemode" = "yes";
        "load printers" = "no";
        printing = "bsd";
        "printcap name" = "/dev/null";
        "disable spoolss" = "yes";
        "username map" = toString (pkgs.writeText "smb-username-map" ludwig.smbUsernameMap);
        "hosts allow" = "192.168.8.0/22 100.64.0.0/10 127.0.0.1";
        "hosts deny" = "0.0.0.0/0";
      };
    }
    // lib.mapAttrs (_: share: shareDefaults // share) ludwig.smbShares;
  };

  services.samba-wsdd = {
    enable = true;
    openFirewall = true;
    workgroup = "WORKGROUP";
    hostname = "truenas";
  };

  # passdb.tdb holds the SMB password hashes imported from TrueNAS.
  environment.persistence."/persist".directories = [ "/var/lib/samba" ];

  # One-time import of the TrueNAS `pdbedit -L -w` export that install.sh
  # places in /persist. Accounts without a Unix user here are skipped.
  systemd.services.samba-import-truenas-passdb = {
    description = "Import SMB password hashes exported from TrueNAS";
    wantedBy = [ "samba-smbd.service" ];
    before = [ "samba-smbd.service" ];
    unitConfig.ConditionPathExists = smbpasswd;
    path = [
      config.services.samba.package
      pkgs.coreutils
      pkgs.getent
    ];
    serviceConfig.Type = "oneshot";
    script = ''
      known=$(mktemp)
      while IFS=: read -r user rest; do
        if getent passwd "$user" >/dev/null; then
          printf '%s:%s\n' "$user" "$rest"
        else
          echo "skipping $user: no such user" >&2
        fi
      done <${smbpasswd} >"$known"
      pdbedit -i "smbpasswd:$known"
      shred -u "$known" ${smbpasswd}
      pdbedit -L
    '';
  };
}
