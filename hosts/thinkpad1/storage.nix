{
  # BUILD-ONLY PLACEHOLDERS, explicitly requested by the operator.
  # Replace with the actual generated filesystem/LUKS/swap settings before install
  # or deployment. These labels deliberately do not describe an existing disk.
  warnings = [
    "thinkpad1 has BUILD-ONLY placeholder filesystems. Do not install or deploy until storage.nix contains the real disk configuration."
  ];
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/PLACEHOLDER-thinkpad1-root";
      fsType = "ext4";
    };
    "/boot" = {
      device = "/dev/disk/by-label/PLACEHOLDER-thinkpad1-efi";
      fsType = "vfat";
      options = [ "umask=0077" ];
    };
  };
  swapDevices = [ ];
}
