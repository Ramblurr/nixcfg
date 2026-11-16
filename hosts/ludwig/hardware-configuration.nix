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
  altroot = "/mnt";
  loadKey = pool: keyFile: ''
    if [ "$(${zfs} get -H -o value keystatus ${pool})" != available ]; then
      ${zfs} load-key -L file://${keyFile} ${pool}
    fi
  '';
in
{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  networking.hostId = site.hostId;

  hardware.cpu.intel.updateMicrocode = true;
  zramSwap.enable = true;

  systemd.settings.Manager = {
    RuntimeWatchdogSec = "60s";
    RebootWatchdogSec = "10min";
  };

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
      # Plaintext on both ESPs: the price of unattended boot.
      secrets.${initrdKey} = "${site.persist.zfsKeys}/${rootPool}.key";
      # Every sysroot mount and the rollback wait for the import unit, so they
      # also wait for the key.
      systemd.services."zfs-import-${rootPool}".postStart = loadKey rootPool initrdKey;
    };
  };

  # tank's stored mountpoints assume this altroot; shares, exports and
  # clients use the resulting paths. The module's own import then finds the
  # pool imported.
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
