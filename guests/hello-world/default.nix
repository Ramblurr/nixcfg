{ pkgs, ... }:

{
  system.stateVersion = "24.11";
  services = {
    nginx = {
      enable = true;
      virtualHosts."hello-world.socozy.casa" = {
        forceSSL = true;
        enableACME = true;
      };
    };
  };

  boot.initrd.systemd = {
    enable = true;
    #services.bootstrap-secrets = {
    #  after = [ "initrd-fs.target" ];
    #  before = [
    #    "initrd-nixos-activation.service"
    #    "shutdown.target"
    #    "initrd-switch-root.target"
    #  ];
    #  conflicts = [
    #    "shutdown.target"
    #    "initrd-switch-root.target"
    #  ];
    #  wantedBy = [ "initrd.target" ];
    #  unitConfig.DefaultDependencies = false;
    #  #serviceConfig.Type = "oneshot";
    #  serviceConfig.ImportCredential = "SOPS_AGE_KEY";
    #  script = ''
    #    echo "Listing creds"
    #    ls -al /run/credentials
    #    ls -al /run/credentials/bootstrap-secrets.service
    #    echo "CREDENTIALS_DIRECTORY=$CREDENTIALS_DIRECTORY"
    #    ls -al $CREDENTIALS_DIRECTORY
    #    systemd-creds --system
    #    mkdir -p /etc/ssh
    #    ${pkgs.systemd}/bin/systemd-creds --system cat SOPS_AGE_KEY > /etc/ssh/ssh_host_ed25519_key
    #    chmod 0600 /etc/ssh/ssh_host_ed25519_key
    #  '';
    #};
  };
}
