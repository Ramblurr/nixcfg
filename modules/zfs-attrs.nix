{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.zfs.datasets;
  serviceDatasets = lib.foldl' (
    servicesByName: dataset:
    lib.foldl' (
      result: service:
      result
      // {
        ${service} = (result.${service} or [ ]) ++ [ dataset ];
      }
    ) servicesByName cfg.services.${dataset}
  ) { } (builtins.attrNames cfg.services);
  datasetMountpoint = dataset: (cfg.properties.${dataset} or { }).mountpoint or "";
  serviceMountpoints = lib.mapAttrs (
    _service: datasets: map datasetMountpoint (lib.unique datasets)
  ) serviceDatasets;
  serviceUnits = lib.mapAttrs (_service: mountpoints: {
    requires = [ "zfs-datasets.service" ];
    after = [ "zfs-datasets.service" ];
    bindsTo = [ "zfs-mount.service" ];
    unitConfig = {
      AssertPathIsMountPoint = mountpoints;
      RequiresMountsFor = mountpoints;
    };
  }) serviceMountpoints;
  registeredDatasets = builtins.attrNames cfg.services;
  missingDatasets = builtins.filter (
    dataset: !(builtins.hasAttr dataset cfg.properties)
  ) registeredDatasets;
  invalidMountpoints = builtins.filter (
    dataset: builtins.hasAttr dataset cfg.properties && !(lib.hasPrefix "/" (datasetMountpoint dataset))
  ) registeredDatasets;
  registeredServices = lib.concatLists (builtins.attrValues cfg.services);
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
      services = lib.mkOption {
        description = "System services that require each native ZFS dataset.";
        default = { };
        example = {
          "rpool/services/postgresql" = [ "postgresql" ];
        };
        type = lib.types.attrsOf (lib.types.listOf lib.types.str);
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = missingDatasets == [ ];
        message = "ZFS dataset services reference undeclared datasets: ${lib.concatStringsSep ", " missingDatasets}";
      }
      {
        assertion = invalidMountpoints == [ ];
        message = "ZFS dataset services require absolute native mountpoints: ${lib.concatStringsSep ", " invalidMountpoints}";
      }
      {
        assertion = lib.all (
          service:
          service != ""
          && !(lib.hasSuffix ".service" service)
          && !(builtins.elem service [
            "zfs-datasets"
            "zfs-datasets-reactivation"
          ])
        ) registeredServices;
        message = "ZFS dataset service names must omit .service and cannot name dataset readiness services.";
      }
    ];
    systemd.services = serviceUnits // {
      zfs-datasets = {
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
        requires = [ "zfs-mount.service" ];
        after = [
          "zfs-mount.service"
          "local-fs.target"
        ];
        unitConfig.DefaultDependencies = false;
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
        };
        # Keep the boot readiness barrier active; reactivation below reapplies changes.
        restartIfChanged = false;
        path = [ pkgs.zfs ];
        script = ''
          dsList=(${toString (lib.mapAttrsToList (ds: _prop: "${ds}") cfg.properties)})

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
      # NixOS runs sysinit reactivation before ordinary reloads. Reapply here so
      # tmpfiles cannot create directories beneath datasets not yet mounted.
      zfs-datasets-reactivation = {
        requiredBy = [
          "sysinit-reactivation.target"
          "systemd-tmpfiles-resetup.service"
        ];
        before = [
          "sysinit-reactivation.target"
          "systemd-tmpfiles-resetup.service"
        ];
        requires = [ "zfs-datasets.service" ];
        after = [ "zfs-datasets.service" ];
        unitConfig.DefaultDependencies = false;
        serviceConfig.Type = "oneshot";
        restartIfChanged = false;
        inherit (config.systemd.services.zfs-datasets) path script;
      };
    };
  };
}
