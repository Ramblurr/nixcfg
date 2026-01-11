{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.modules.services.sshd;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.services.sshd = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };
  };
  config = mkIf cfg.enable {
    # Should exist already as it is used for sops bootstrapping
    # sops.secrets.ssh_host_ed25519_key = {
    #   path = "/persist/etc/ssh/ssh_host_ed25519_key";
    # };

    sops.secrets.ssh_host_ed25519_key_pub = {
      path = "${lib.optionalString withImpermanence "/persist"}/etc/ssh/ssh_host_ed25519_key.pub";
    };
    networking.firewall.allowedTCPPorts = [ 22 ];

    services.openssh = {
      enable = true;
      authorizedKeysFiles = lib.mkForce [ "/etc/ssh/authorized_keys.d/%u" ];
      settings = {
        # AcceptEnv type changed: string in 25.11 (stable), list in 26.05+ (unstable)
        AcceptEnv =
          if lib.versionAtLeast config.system.nixos.release "26.05" then
            [ "SYSTEMD_PAGER" ]
          else
            "SYSTEMD_PAGER";
        LoginGraceTime = 30;
        PermitRootLogin = lib.mkOverride 900 "prohibit-password";
        PasswordAuthentication = false;
        KbdInteractiveAuthentication = false;
        StreamLocalBindUnlink = true;
      };
      hostKeys = [
        {
          path = "${lib.optionalString withImpermanence "/persist"}/etc/ssh/ssh_host_ed25519_key";
          type = "ed25519";
        }
      ];
    };

    environment.persistence."/persist" = mkIf withImpermanence {
      files = [ "${config.users.users.root.home}/.ssh/known_hosts" ];
    };
  };
}
