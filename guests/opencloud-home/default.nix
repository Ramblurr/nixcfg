{
  config,
  inputs,
  lib,
  ...
}:
{
  imports = [
    inputs.quadlet-nix2.nixosModules.default
    ../../modules/services/opencloud.nix
  ];

  networking.hostName = "opencloud-home";
  system.stateVersion = "26.05";
  modules = {
    microvm-guest = {
      host = "dewey";
      hostFQDN = "dewey.${config.site.net.svc.domainName}";
    };
    users.enable = lib.mkForce false;
    services.sshd.enable = lib.mkForce false;
  };
  microvm = {
    # QEMU supports the host-supplied systemd credentials used by this guest.
    hypervisor = "qemu";
    mem = 8192;
    vcpu = 4;
    vsock.ssh.enable = false;
    volumes = [
      {
        # Host backing is a ZFS dataset below the existing safe/vms backup root.
        # Use a guest block filesystem, not virtiofs, for Podman graph storage.
        image = "/var/lib/opencloud-home-vm/var.img";
        mountPoint = "/var";
        fsType = "ext4";
        size = 128 * 1024;
      }
    ];
  };
  fileSystems."/var".neededForBoot = true;
  boot.supportedFilesystems = [ "nfs" ];
  networking.nftables.enable = true;

  # Do not inherit development guests' empty passwords or require guest SOPS
  # bootstrapping. The host passes application credentials at VM startup.
  users.users.root.hashedPassword = "!";
  services.openssh = {
    enable = true;
    openFirewall = false;
    hostKeys = [
      {
        type = "ed25519";
        path = "/var/lib/ssh/ssh_host_ed25519_key";
      }
    ];
    settings = {
      PermitRootLogin = "prohibit-password";
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
      PermitEmptyPasswords = false;
    };
  };
}
