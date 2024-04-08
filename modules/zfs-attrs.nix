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
      requiredBy = [
        "systemd-tmpfiles-setup.service"
        "systemd-tmpfiles-resetup.service"
        "sysinit.target"
        "sysinit-reactivation.target"
      ];
      before = [
        "sysinit.target"
        "sysinit-reactivation.target"
        "systemd-tmpfiles-setup.service"
        "systemd-tmpfiles-resetup.service"
      ];
      after = [ "local-fs.target" ];
      unitConfig.DefaultDependencies = false;
      serviceConfig = {
        Type = "oneshot";
      };
      restartIfChanged = true;
      restartTriggers = [ config.systemd.services.zfs-datasets.script ];
      path = [ pkgs.zfs ];
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
