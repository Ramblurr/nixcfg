{
  options,
  config,
  lib,
  pkgs,
  inputs,
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
    permitRootLogin.enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    # The host key :
    #     /persist/etc/ssh/ssh_host_ed25519_key
    #     /persist/etc/ssh/ssh_host_ed25519_key.pub
    # is placed there at boostrap time when we install nix using nixos anywhere.

    sops.secrets.ssh_host_ed25519_key_pub =
      lib.mkIf (config.modules.users.sops.enable && !config.modules.users.age.enable)
        {
          path = "${lib.optionalString withImpermanence "/persist"}/etc/ssh/ssh_host_ed25519_key.pub";
        };

    services.openssh = {
      enable = true;
      authorizedKeysFiles = lib.mkForce [ "/etc/ssh/authorized_keys.d/%u" ];
      settings = {
        StreamLocalBindUnlink = true;
        PasswordAuthentication = false;
        KbdInteractiveAuthentication = false;
        PermitRootLogin = "yes";
      };
      hostKeys = [
        {
          path = "${lib.optionalString withImpermanence "/persist"}/etc/ssh/ssh_host_ed25519_key";
          type = "ed25519";
        }
      ];
    };

    environment.persistence."/persist" = mkIf withImpermanence {
      files = [ "/root/.ssh/known_hosts" ];
    };
  };
}
