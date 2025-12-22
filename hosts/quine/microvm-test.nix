{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.home-ops.microvm-host;
in
{
  options.home-ops.microvm-host = {
    enable = lib.mkEnableOption "Enable microvm host services.";
    baseZfsDataset = lib.mkOption {
      type = lib.types.str;
      description = "Base ZFS dataset whereunder to create shares for MicroVMs.";
    };
  };
  config = lib.mkIf cfg.enable {
    # microvm host settings
    microvm = {
      host.enable = true;
      autostart = [ ];
    };
    # allow microvm access to zvol
    users.users.microvm.extraGroups = [ "disk" ];

    systemd.services = {

      "microvm-virtiofsd@" = {
        requires = [ "microvm-zfs-datasets@%i.service" ];
      };

      "microvm-zfs-datasets@" = {
        description = "Create ZFS datasets for MicroVM '%i'";
        before = [ "microvm-virtiofsd@%i.service" ];
        after = [ "local-fs.target" ];
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
