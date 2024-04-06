{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.zfs.datasets;
in
{
  options = {
    modules.zfs.datasets = {
      enable = lib.mkEnableOption "declarative ZFS dataset properties";
      properties = lib.mkOption {
        description = lib.mdDoc ''
          Declarative ZFS dataset properties.
          ZFS dataset property value for <literal>zfs set</literal>.
          zfs filesystem is created if it does not exist.
          Does not delete anything if an property is removed.
        '';
        example = ''
          {
            "rpool/home"."com.sun:auto-snapshot" = "true";
            "rpool/root".quota = "100G";
          }
        '';
        default = { };
        type = with lib.types; attrsOf (attrsOf str);
      };
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.zfs-datasets = {
      path = [ pkgs.zfs ];
      restartIfChanged = true;

      wantedBy = [ "local-fs.target" ];
      before = [ "systemd-tmpfiles-setup.service" ];
      requires = [ "zfs.target" ];
      restartTriggers = [ config.systemd.services.zfs-datasets.script ];

      # TODO(24.05) switch to these after unstable becomes 24.05
      # wantedBy = [ "local-fs.target" ];
      # requiredBy = [ "sysinit-reactivation.target" ];
      # before = [ "systemd-tmpfiles-setup.service"  "systemd-tmpfiles-resetup.service" "sysinit-reactivation.target"];
      # requires = [ "zfs.target" ];

      serviceConfig = {
        Type = "oneshot";
      };

      script = ''
        dsList=(${toString (lib.mapAttrsToList (ds: prop: "${ds}") cfg.properties)})

        # Create datasets if neccesary
        for ds in "''${dsList[@]}"; do
          res=$(zfs list "$ds" 2> /dev/null > /dev/null || echo create)
          if [ "$res" == "create" ]; then
            echo "creating $s"
            zfs create -p "$ds"
          fi
        done


        ${lib.concatStringsSep "\n" (
          lib.flatten (
            lib.mapAttrsToList (
              ds: prop:
              lib.mapAttrsToList (key: val: ''
                if [ $(zfs get -H ${key} ${ds} | ${pkgs.gawk}/bin/awk '{ print $3 }') != "${val}" ]; then
                  zfs set ${key}=${val} ${ds}
                fi
              '') prop
            ) cfg.properties
          )
        )}
      '';
    };
  };
}
