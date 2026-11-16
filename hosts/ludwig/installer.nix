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
# truenas-stage.sh, run on TrueNAS, writes a bundle to ludwig-installer/ on the
# TrueNAS ESP: the tailscale-ludwig jail state, the migration inputs and an
# optional autoinstall marker. This image copies it into RAM at boot. With the
# marker present, ludwig-autoinstall runs every step, wipes the bundle and
# reboots into NixOS once. See README.md.
let
  cfg = config.ludwigInstaller;
  truenasEsp = "/dev/disk/by-partuuid/d4fb2736-d81c-11ec-bff8-d05099fb0f82";
  seedDir = "/run/ludwig-installer";
  install = pkgs.writeShellApplication {
    name = "ludwig-install";
    runtimeInputs = [
      config.boot.zfs.package
      config.nix.package
      config.systemd.package
      pkgs.coreutils
      pkgs.dosfstools
      pkgs.efibootmgr
      pkgs.findutils
      pkgs.gawk
      pkgs.gnugrep
      pkgs.gnused
      pkgs.gptfdisk
      pkgs.ipmitool
      pkgs.nixos-install-tools
      pkgs.util-linux
    ];
    runtimeEnv = lib.optionalAttrs (cfg.system != null) { LUDWIG_SYSTEM = "${cfg.system}"; };
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
    # TrueNAS kern.hostid; see hardware-configuration.nix.
    networking.hostId = "ac91e159";
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
    networking.nameservers = [
      "1.1.1.1"
      "9.9.9.9"
    ];
    # Whichever port has a cable gets the known address, so SSH works through
    # plato's subnet route even if this node never joins Tailscale. Ports
    # without carrier are left unconfigured.
    systemd.network.networks."10-lan" = {
      matchConfig.Name = "en*";
      address = [ "192.168.8.4/22" ];
      gateway = [ "192.168.8.1" ];
      networkConfig.DHCP = "ipv4";
      networkConfig.IPv6AcceptRA = false;
    };

    networking.firewall.trustedInterfaces = [ "tailscale0" ];
    services.tailscale = {
      enable = true;
      useRoutingFeatures = "server";
      # Only used when no jail state was provided; use a tagged auth key.
      authKeyFile = "${seedDir}/authkey";
      # Keep the LAN, including the BMC, reachable while TrueNAS is down.
      extraSetFlags = [
        "--advertise-routes=192.168.8.0/22"
        "--accept-dns=false"
        "--accept-routes=false"
      ];
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
        install -d -m 0700 ${seedDir} /var/lib/tailscale
        mnt=$(mktemp -d)
        mount -o ro ${truenasEsp} "$mnt"
        src="$mnt/ludwig-installer"
        if [ -f "$src/tailscaled.state" ] && [ ! -e /var/lib/tailscale/tailscaled.state ]; then
          install -m 0600 "$src/tailscaled.state" /var/lib/tailscale/tailscaled.state
        fi
        if [ -f "$src/authkey" ]; then
          install -m 0600 "$src/authkey" ${seedDir}/authkey
        fi
        if [ -d "$src/migration" ]; then
          rm -rf /root/ludwig-migration
          cp -r "$src/migration" /root/ludwig-migration
          chmod -R go= /root/ludwig-migration
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
      pkgs.efibootmgr
      pkgs.gptfdisk
      pkgs.ipmitool
      pkgs.jq
      pkgs.smartmontools
      pkgs.tmux
    ];
  };
}
