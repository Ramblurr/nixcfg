{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  cfg = config.modules.microvm-host;
in
{
  options.modules.microvm-host = {
    enable = lib.mkEnableOption "Enable microvm host services (for imperative control!)";
    baseZfsDataset = lib.mkOption {
      type = lib.types.str;
      description = "Base ZFS dataset whereunder to create shares for MicroVMs.";
    };
  };
  config = lib.mkIf cfg.enable {
    microvm = {
      host.enable = true;
      # TODO autostart = [ ];
    };

    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/microvms"."mountpoint" = "/var/lib/microvms";
      "rpool/encrypted/safe/svc/microvms"."com.sun:auto-snapshot" = "false";
    };
    systemd.tmpfiles.rules = [ "d /var/lib/microvms 0770 microvm kvm" ];

    # allow microvm access to zvol
    users.users.microvm.extraGroups = [ "disk" ];

    # systemd services to ensure the ZFS datasets for the microvms are created
    systemd.services = {
      "microvm-virtiofsd@" = {
        requires = [ "microvm-zfs-datasets@%i.service" ];
      };
      "microvm-zfs-datasets@" = {
        description = "Create ZFS datasets for MicroVM '%i'";
        before = [ "microvm-virtiofsd@%i.service" ];
        after = [
          "local-fs.target"
          "zfs-datasets.service"
        ];
        partOf = [ "microvm@%i.service" ];
        unitConfig.ConditionPathExists = "/var/lib/microvms/%i/current/share/microvm/virtiofs";
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          WorkingDirectory = "/var/lib/microvms/%i";
          SyslogIdentifier = "microvm-zfs-datasets@%i";
        };
        path = with pkgs; [ zfs ];
        scriptArgs = "%i";
        script = # bash
          ''
            zfsExists() {
              zfs list $1 >/dev/null 2>/dev/null
            }

            NAME="$1"
            BASE="${cfg.baseZfsDataset}"
            zfsExists $BASE || \
              zfs create $BASE
            zfsExists $BASE/$NAME || \
              zfs create $BASE/$NAME
            for d in current/share/microvm/virtiofs/*; do
              SOURCE=$(cat $d/source)
              TAG=$(basename $d)
              MNT=$SOURCE
              if [[ "$MNT" == /var/lib/microvms/$NAME/* ]]; then
                zfsExists $BASE/$NAME/$TAG || \
                  zfs create -o mountpoint=$MNT $BASE/$NAME/$TAG
              fi
            done
          '';
      };
    };

    nix.settings = {
      min-free =
        10 # gb
        * 1024
        * 1024
        * 1024;
      max-free =
        20 # gb
        * 1024
        * 1024
        * 1024;
    };
  };
}
