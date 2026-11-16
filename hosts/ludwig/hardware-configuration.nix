{
  config,
  lib,
  modulesPath,
  ...
}:
let
  zfs = lib.getExe' config.boot.zfs.package "zfs";
  zpool = lib.getExe' config.boot.zfs.package "zpool";
  # Pool keys live on the encrypted fast pool; install.sh places them.
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

  # TrueNAS kern.hostid, recorded in both pool labels. Matching it lets either
  # OS import the pools without -f after an unclean shutdown.
  networking.hostId = "ac91e159";

  hardware.cpu.intel.updateMicrocode = true;
  zramSwap.enable = true;

  # Reboot a hung remote host instead of waiting for someone on site.
  systemd.settings.Manager = {
    RuntimeWatchdogSec = "60s";
    RebootWatchdogSec = "10min";
  };

  fileSystems."/" = {
    device = "fast/encrypted/local/root";
    fsType = "zfs";
    neededForBoot = true;
  };
  fileSystems."/nix" = {
    device = "fast/encrypted/local/nix";
    fsType = "zfs";
    neededForBoot = true;
  };
  fileSystems."/home" = {
    device = "fast/encrypted/local/home";
    fsType = "zfs";
    neededForBoot = true;
  };
  fileSystems."/persist" = {
    device = "fast/encrypted/safe/persist";
    fsType = "zfs";
    neededForBoot = true;
  };

  # One ESP on partition 1 of each fast SSD (formerly TrueNAS swap).
  fileSystems."/boot" = {
    device = "/dev/disk/by-partlabel/ludwig-esp0";
    fsType = "vfat";
    options = [
      "fmask=0077"
      "dmask=0077"
    ];
  };
  fileSystems."/boot-fallback" = {
    device = "/dev/disk/by-partlabel/ludwig-esp1";
    fsType = "vfat";
    options = [
      "fmask=0077"
      "dmask=0077"
      "nofail"
    ];
  };

  boot = {
    loader = {
      efi.canTouchEfiVariables = true;
      grub = {
        enable = true;
        efiSupport = true;
        copyKernels = true;
        configurationLimit = 10;
        mirroredBoots = [
          {
            path = "/boot";
            devices = [ "nodev" ];
          }
          {
            path = "/boot-fallback";
            devices = [ "nodev" ];
          }
        ];
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
      secrets."/etc/zfs-keys/fast.key" = "/persist/etc/zfs-keys/fast.key";
      # Every sysroot mount and the rollback wait for the import unit, so they
      # also wait for the key.
      systemd.services.zfs-import-fast.postStart = loadKey "fast" "/etc/zfs-keys/fast.key";
    };
  };

  # Never change pool properties: TrueNAS must find both pools exactly as it
  # left them, or a fallback boot cannot start its jails, Tailscale included.
  # A temporary altroot gives tank the TrueNAS paths (/mnt/tank) instead; the
  # module's own import then finds the pool imported. fast mounts its
  # non-NixOS datasets (the old iocage jails) under /fast, unused.
  systemd.services.zfs-import-tank = {
    preStart = ''
      for _ in $(seq 1 60); do
        ${zpool} list tank >/dev/null 2>&1 && break
        ${zpool} import -d ${config.boot.zfs.devNodes} -N -R /mnt -o cachefile=none tank && break
        sleep 1
      done
      # Fail rather than let the module import tank at /tank.
      ${zpool} list -H -o altroot tank | grep -qx /mnt
    '';
    postStart = loadKey "tank" "/persist/etc/zfs-keys/tank.key";
  };
}
