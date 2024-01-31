{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.boot.zfs;
in {
  options = {
    modules.boot.zfs = {
      enable = mkBoolOpt false;
      encrypted = mkBoolOpt true;
      usePlymouth = mkBoolOpt true;
      skipMitigations = mkBoolOpt false;
      autoSnapshot.enable = mkBoolOpt true;
      rootPool = mkStrOpt "rpool";
      scrubPools = mkOption {
        type = types.listOf types.str;
        default = ["rpool"];
      };
      extraPools = mkOption {
        type = types.listOf types.str;
        default = [];
      };
    };
  };

  config = mkIf cfg.enable {
    ## BOOT #################################################################
    ## WARNING this module makes assumptions about your disk partition layout
    console.earlySetup = true; # needed for LUKS
    boot = {
      tmp.useTmpfs = mkDefault false;
      tmp.cleanOnBoot = true;
      zfs.enableUnstable = false;
      zfs.requestEncryptionCredentials = cfg.encrypted;
      zfs.extraPools = cfg.extraPools;

      plymouth.enable = cfg.usePlymouth;
      initrd = {
        supportedFilesystems = ["zfs"];
        # NOTE: 2023-04-24
        # I couldn't get initrd.systemd to work the rollback service failed with
        # "The ZFS Modules are not loaded"
        systemd.enable = false;
        systemd.services.rollback = mkIf (config.boot.initrd.systemd.enable) {
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
          script =
            mkIf config.modules.impermanence.enable
            (
              if cfg.encrypted
              then ''
                zfs rollback -r ${cfg.rootPool}/encrypted/local/root@blank && \
                zfs rollback -r ${cfg.rootPool}/encrypted/local/home@blank && \
                echo "rollback complete"
              ''
              else ''
                zfs rollback -r ${cfg.rootPool}/local/root@blank && \
                zfs rollback -r ${cfg.rootPool}/local/home@blank && \
                echo "rollback complete"
              ''
            );
        };
      };
      kernelModules = ["kvm-intel"];
      extraModulePackages = [];

      kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;
      kernelParams = mkIf cfg.skipMitigations ["mitigations=off"];
      kernel.sysctl = {
        "fs.file-max" = 1048576;
        "fs.inotify.max_user_instances" = 256;
        "fs.inotify.max_user_watches" = 99999999;
      };
    };

    ## LEGACYBOOT - we use stage-1/systemd so have a fallback ###############
    #specialisation."legacyboot" = lib.mkIf (config.boot.initrd.systemd.enable) {
    #  inheritParentConfig = true;
    #  configuration = {
    #    boot.initrd.systemd.enable = lib.mkForce false;
    #  };
    #};

    services.fwupd.enable = true;
    services.udisks2.enable = true;
    hardware.enableRedistributableFirmware = true;
    hardware.usb-modeswitch.enable = false; # dual role usb/cdrom stick thing
    hardware.cpu.amd.updateMicrocode = pkgs.hostPlatform.system == "x86_64-linux";
    hardware.cpu.intel.updateMicrocode = pkgs.hostPlatform.system == "x86_64-linux";

    environment = {
      systemPackages = with pkgs; [
        coreutils
      ];
      etc."flake.lock" = {
        source = ../../flake.lock;
      };
    };

    ## SILLY CUSTOMIZATION ##################################################
    services.getty = {
      greetingLine = ''\l  -  (kernel: \r) (label: ${config.system.nixos.label}) (arch: \m)'';
      helpLine = ''
      '';
    };

    environment.persistence."/persist" = mkIf config.modules.impermanence.enable {
      hideMounts = true;
      directories = [
        "/etc/nixos"
        "/var/log"
        "/var/lib/cups"
        #"/var/lib"
        #"/srv"
      ];
      files = [
        "/var/lib/dbus/machine-id"
      ];
    };
    programs.fuse.userAllowOther = true;

    # run this to enable auto-trim on the pool:
    # `sudo zpool set autotrim=on TANK`

    # check if its enabled:
    # zpool get all | grep autotrim (also check if luks is allowDiscards)

    services.zfs = {
      trim.enable = true;
      autoScrub = {
        enable = true;
        pools = cfg.scrubPools;
      };
      autoSnapshot = {
        enable = cfg.autoSnapshot.enable;
        frequent = 8; # keep the latest eight 15-minute snapshots (instead of four)
        monthly = 1; # keep only one monthly snapshot (instead of twelve)
      };
    };
  };
}
