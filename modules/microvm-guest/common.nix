{
  config,
  lib,
  pkgs,
  ...
}:

{
  boot = {
    loader.grub.enable = false;
    initrd.kernelModules = [
      # required for net.netfilter.nf_conntrack_max appearing in sysfs early at boot
      "nf_conntrack"
    ];
    kernel.sysctl =
      let
        mem = config.microvm.mem;
      in
      lib.optionalAttrs (mem <= 2 * 1024) {
        # table overflow causing packets from nginx to the service to drop
        # nf_conntrack: nf_conntrack: table full, dropping packet
        "net.netfilter.nf_conntrack_max" = lib.mkDefault "65536";
      };
    kernelParams = [
      # mitigations which cost the most performance and are the least real world relevant
      # NOTE: keep in sync with baremetal.nix
      "retbleed=off"
      "gather_data_sampling=off" # Downfall
    ];
  };

  fileSystems."/" = lib.mkDefault {
    fsType = "tmpfs";
  };

  hardware.enableRedistributableFirmware = false;

  # nix store is mounted read only
  nix = {
    enable = lib.mkDefault false;
    gc.automatic = false;
    optimise.automatic = false;
  };

  system.build.installBootLoader = "${pkgs.coreutils}/bin/true";

  systemd.tmpfiles.rules = [
    "d /home/root 0700 root root -" # createHome does not create it
  ];

  users = {
    mutableUsers = false;
    users."root" = {
      createHome = true;
      #home = lib.mkForce "/home/root";
    };
  };
}
