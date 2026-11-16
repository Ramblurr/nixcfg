{
  config,
  lib,
  modulesPath,
  ...
}:
let
  site = import ./site.nix;
  inherit (site) rootPool;
  zfs = lib.getExe' config.boot.zfs.package "zfs";
  zpool = lib.getExe' config.boot.zfs.package "zpool";
  dataset = name: {
    device = "${rootPool}/encrypted/${name}";
    fsType = "zfs";
    neededForBoot = true;
  };
  esp = i: esp: {
    name = esp.mountPoint;
    value = {
      device = "/dev/disk/by-partlabel/${esp.label}";
      fsType = "vfat";
      options = [
        "fmask=0077"
        "dmask=0077"
      ]
      ++ lib.optional (i > 0) "nofail";
    };
  };
  initrdKey = "/etc/zfs-keys/${rootPool}.key";
  # The TrueNAS altroot.
  altroot = "/mnt";
  # Pool keys live on the encrypted root pool; install.sh places them.
  # TrueNAS loaded them from its config database, keylocation stays "prompt"
  # so the pools remain importable by TrueNAS for rollback.
  loadKey = pool: keyFile: ''
    if [ "$(${zfs} get -H -o value keystatus ${pool})" != available ]; then
      ${zfs} load-key -L file://${keyFile} ${pool}
    fi
  '';
in
{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  # Matching TrueNAS lets either OS import the pools without -f after an
  # unclean shutdown.
  networking.hostId = site.hostId;

  hardware.cpu.intel.updateMicrocode = true;
  zramSwap.enable = true;

  # Reboot a hung remote host instead of waiting for someone on site.
  systemd.settings.Manager = {
    RuntimeWatchdogSec = "60s";
    RebootWatchdogSec = "10min";
  };

  # The ESPs are the former TrueNAS swap partitions.
  fileSystems = lib.mapAttrs (_: dataset) site.datasets // lib.listToAttrs (lib.imap0 esp site.esps);

  boot = {
    loader = {
      efi.canTouchEfiVariables = true;
      grub = {
        enable = true;
        efiSupport = true;
        copyKernels = true;
        configurationLimit = 10;
        mirroredBoots = map (esp: {
          path = esp.mountPoint;
          devices = [ "nodev" ];
        }) site.esps;
      };
    };

    # Keys are loaded by the units below rather than prompted for.
    zfs.requestEncryptionCredentials = lib.mkForce false;

    initrd = {
      availableKernelModules = [
        "ahci"
        "sd_mod"
        "usb_storage"
        "usbhid"
        "xhci_pci"
      ];
      # Plaintext on both ESPs. TrueNAS kept the same key on its boot device,
      # so unattended boot keeps the existing protection level.
      secrets.${initrdKey} = "${site.persist.zfsKeys}/${rootPool}.key";
      # Every sysroot mount and the rollback wait for the import unit, so they
      # also wait for the key.
      systemd.services."zfs-import-${rootPool}".postStart = loadKey rootPool initrdKey;
    };
  };

  # Never change pool properties: TrueNAS must find both pools exactly as it
  # left them, or a fallback boot cannot start its jails, Tailscale included.
  # A temporary altroot gives tank its TrueNAS paths instead; the module's
  # own import then finds the pool imported. The root pool's old TrueNAS
  # datasets (the iocage jails) mount at their stored mountpoints, unused.
  systemd.services.zfs-import-tank = {
    preStart = ''
      for _ in $(seq 1 60); do
        ${zpool} list tank >/dev/null 2>&1 && break
        ${zpool} import -d ${config.boot.zfs.devNodes} -N -R ${altroot} -o cachefile=none tank && break
        sleep 1
      done
      # Fail rather than let the module import tank at its stored mountpoint.
      ${zpool} list -H -o altroot tank | grep -qx ${altroot}
    '';
    postStart = loadKey "tank" "${site.persist.zfsKeys}/tank.key";
  };
}
