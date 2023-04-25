{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  boot.initrd.systemd.services.rollback = lib.mkIf (config.boot.initrd.systemd.enable) {
    description = "Rollback ZFS datasets to a pristine state";
    wantedBy = [
      "initrd.target"
    ];
    after = [
      "zfs-import.target"
    ];
    before = [
      "sysroot.mount"
    ];
    path = with pkgs; [
      zfs
    ];
    unitConfig.DefaultDependencies = "no";
    serviceConfig.Type = "oneshot";
    #cryptsetup close /dev/mapper/cryptkey && \
    script = ''
      zfs rollback -r rpool/encrypted/local/root@blank && \
      zfs rollback -r rpool/encrypted/local/home@blank && \
      echo "rollback complete"
    '';
  };

  environment.persistence."/persist" = {
    hideMounts = true;
    directories = [
      "/etc/nixos"
      "/var/log"
      #"/var/lib"
      #"/srv"
    ];
    files = [
      "/var/lib/dbus/machine-id"
    ];
  };
  programs.fuse.userAllowOther = true;
}
