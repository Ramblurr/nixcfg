{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}:
#
# Installer ISO for the TrueNAS-to-NixOS migration. It joins the tailnet on its
# own, so the remote site stays reachable while TrueNAS is down.
#
# truenas-stage.sh, run on TrueNAS, writes a bundle to the TrueNAS ESP: the
# Tailscale auth key, the migration inputs and an optional autoinstall
# marker. This image copies it into RAM at boot. With the marker
# present, ludwig-autoinstall runs every step, wipes the bundle and reboots
# into NixOS once. See README.md.
let
  cfg = config.ludwigInstaller;
  site = import ./site.nix;
  truenasEsp = "/dev/disk/by-partuuid/${site.truenasEspUuid}";
  seedDir = "/run/ludwig-installer";
  inputDir = "/root/ludwig-migration";
  ludwigBoot = pkgs.callPackage ./ludwig-boot.nix { };
  install = pkgs.writeShellApplication {
    name = "ludwig-install";
    runtimeInputs = [
      config.boot.zfs.package
      config.nix.package
      config.systemd.package
      pkgs.coreutils
      pkgs.dosfstools
      pkgs.findutils
      pkgs.gawk
      pkgs.gnugrep
      pkgs.gnused
      pkgs.gptfdisk
      pkgs.ipmitool
      pkgs.nixos-install-tools
      pkgs.util-linux
      ludwigBoot
    ];
    runtimeEnv = {
      LUDWIG_INPUT = inputDir;
      LUDWIG_ROOT_POOL = site.rootPool;
      LUDWIG_DATASETS = toString (lib.mapAttrsToList (mp: ds: "${mp}=${ds}") site.datasets);
      LUDWIG_ESPS = toString (map (esp: "${esp.mountPoint}=${esp.label}") site.esps);
      LUDWIG_TRUENAS_ESP_UUID = site.truenasEspUuid;
      LUDWIG_BUNDLE_DIR = site.bundleDir;
      LUDWIG_SEED_AUTHKEY = config.services.tailscale.authKeyFile;
      LUDWIG_ZFS_KEYS = site.persist.zfsKeys;
      LUDWIG_SSH_HOST_KEYS = site.persist.sshHostKeys;
      LUDWIG_UPS = site.persist.ups;
      LUDWIG_SMBPASSWD = site.persist.smbpasswd;
      LUDWIG_TS_AUTHKEY = site.persist.tailscaleAuthKey;
    }
    // lib.optionalAttrs (cfg.system != null) { LUDWIG_SYSTEM = "${cfg.system}"; };
    text = builtins.readFile ./install.sh;
  };
in
{
  imports = [
    (modulesPath + "/installer/cd-dvd/installation-cd-minimal.nix")
    ./tailscale-watchdog.nix
  ];

  options.ludwigInstaller = {
    authorizedKeys = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      description = "SSH keys allowed to log in as root on the installer";
    };
    system = lib.mkOption {
      type = lib.types.nullOr lib.types.package;
      default = null;
      description = "ludwig system closure to carry in the ISO and install";
    };
    autoInstallDelaySec = lib.mkOption {
      type = lib.types.ints.unsigned;
      default = 180;
      description = "Grace period before the unattended install starts; stop ludwig-autoinstall to abort";
    };
  };

  config = {
    system.stateVersion = "26.05";
    networking.hostName = "ludwig-installer";
    networking.hostId = site.hostId;
    boot.zfs.forceImportRoot = false;
    boot.kernelModules = [
      "ipmi_devintf"
      "ipmi_si"
    ];

    isoImage.squashfsCompression = "zstd -Xcompression-level 6";
    isoImage.storeContents = lib.optional (cfg.system != null) cfg.system;
    image.baseName = lib.mkForce "ludwig-installer-${config.system.nixos.label}";

    nix.settings.experimental-features = [
      "nix-command"
      "flakes"
    ];

    users.users.root.openssh.authorizedKeys.keys = cfg.authorizedKeys;
    services.openssh.settings = {
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
    };

    # Same addressing as the installed host, plus DHCP as a fallback.
    # The minimal profile enables NetworkManager; keep networkd the only manager.
    networking.networkmanager.enable = lib.mkForce false;
    networking.useNetworkd = true;
    networking.nameservers = site.nameservers;
    # Whichever port has a cable gets the known address, so SSH works through
    # plato's subnet route even if this node never joins Tailscale. Ports
    # without carrier are left unconfigured.
    systemd.network.networks."10-lan" = {
      matchConfig.Name = "en*";
      address = [ "${site.lan.address}/${toString site.lan.prefixLength}" ];
      gateway = [ site.lan.gateway ];
      networkConfig.DHCP = "ipv4";
      networkConfig.IPv6AcceptRA = false;
    };

    networking.firewall.trustedInterfaces = [ "tailscale0" ];
    services.tailscale = {
      enable = true;
      useRoutingFeatures = "server";
      # A tagged, pre-approved auth key from the bundle.
      authKeyFile = "${seedDir}/authkey";
      extraSetFlags = site.tailscaleFlags;
    };

    systemd.services.ludwig-installer-seed = {
      description = "Copy the staged migration bundle from the TrueNAS ESP";
      wantedBy = [ "tailscaled.service" ];
      before = [ "tailscaled.service" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
      };
      path = [
        pkgs.coreutils
        pkgs.util-linux
      ];
      script = ''
        for _ in $(seq 30); do
          [ -e ${truenasEsp} ] && break
          sleep 1
        done
        if [ ! -e ${truenasEsp} ]; then
          echo "TrueNAS ESP not found; tailscale needs a manual login" >&2
          exit 0
        fi
        install -d -m 0700 ${seedDir}
        mnt=$(mktemp -d)
        mount -o ro ${truenasEsp} "$mnt"
        src="$mnt/${site.bundleDir}"
        if [ -f "$src/authkey" ]; then
          install -m 0600 "$src/authkey" ${seedDir}/authkey
        fi
        if [ -d "$src/migration" ]; then
          rm -rf ${inputDir}
          cp -r "$src/migration" ${inputDir}
          chmod -R go= ${inputDir}
        fi
        if [ -f "$src/autoinstall" ]; then
          touch ${seedDir}/autoinstall
        fi
        umount "$mnt"
        rmdir "$mnt"
      '';
    };

    systemd.services.ludwig-autoinstall = {
      description = "Unattended TrueNAS-to-NixOS migration";
      wantedBy = [ "multi-user.target" ];
      requires = [ "ludwig-installer-seed.service" ];
      after = [
        "ludwig-installer-seed.service"
        "tailscaled.service"
      ];
      unitConfig.ConditionPathExists = "${seedDir}/autoinstall";
      path = [ "/run/current-system/sw" ];
      serviceConfig = {
        Type = "oneshot";
        TimeoutStartSec = "infinity";
        ExecStartPre = "${pkgs.coreutils}/bin/sleep ${toString cfg.autoInstallDelaySec}";
        ExecStart = "${lib.getExe install} auto";
      };
    };

    environment.systemPackages = [
      install
      ludwigBoot
      pkgs.efibootmgr
      pkgs.gptfdisk
      pkgs.ipmitool
      pkgs.jq
      pkgs.smartmontools
      pkgs.tmux
    ];
  };
}
