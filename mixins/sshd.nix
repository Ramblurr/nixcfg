{
  config,
  lib,
  pkgs,
  ...
}: {
  config = {
    # Should exist already as it is used for sops bootstrapping
    # sops.secrets.ssh_host_ed25519_key = {
    #   path = "/persist/etc/ssh/ssh_host_ed25519_key";
    # };

    sops.secrets.ssh_host_ed25519_key_pub = {
      path = "/persist/etc/ssh/ssh_host_ed25519_key.pub";
    };

    sops.secrets.ssh_host_rsa_key = {
      path = "/persist/etc/ssh/ssh_host_rsa_key";
    };

    sops.secrets.ssh_host_rsa_key_pub = {
      path = "/persist/etc/ssh/ssh_host_rsa_key.pub";
    };

    environment.persistence."/persist" = {
      files = [
        "/etc/ssh/ssh_host_ed25519_key"
        "/etc/ssh/ssh_host_ed25519_key.pub"
        "/etc/ssh/ssh_host_rsa_key"
        "/etc/ssh/ssh_host_rsa_key.pub"
      ];
    };
    networking.firewall.allowedTCPPorts = [22];

    services.openssh = {
      enable = true;
      settings = {
        PermitRootLogin = lib.mkForce "no";
        PasswordAuthentication = false;
      };
    };
  };
}
