{
  config,
  lib,
  ...
}:
{
  options.modules.microvm-guest.vsockOnlySsh = lib.mkEnableOption "root SSH from the VM host over VSOCK only, with no TCP listener";

  config = lib.mkIf config.modules.microvm-guest.vsockOnlySsh {
    modules.services.sshd.enable = lib.mkForce false;
    services.openssh = {
      startWhenNeeded = true;
      openFirewall = false;
      hostKeys = [
        {
          path = "/var/lib/ssh/ssh_host_ed25519_key";
          type = "ed25519";
        }
      ];
      settings = {
        PermitRootLogin = "prohibit-password";
        PasswordAuthentication = false;
        KbdInteractiveAuthentication = false;
      };
    };
    systemd.sockets.sshd.enable = false;
    systemd.services."sshd-vsock@" = {
      wants = [ "sshd-keygen.service" ];
      after = [ "sshd-keygen.service" ];
    };
  };
}
