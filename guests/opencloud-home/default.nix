{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  home = config.repo.secrets.home-ops;
  address = builtins.head config.site.net.svc.hosts4.opencloud-home;
  deweyAddress = builtins.head config.site.net.svc.hosts4.dewey;
  dataSource = config.modules.services.opencloud.instances.home.dataSource;
  user = "opencloud-home";
  ingressNames = [
    "data.${home.homeDomain}"
    "euro-office.${home.homeDomain}"
  ];
in
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
      hostSecrets = {
        enable = true;
        services = [ "opencloud-home-credentials" ];
      };
    };
    users.enable = lib.mkForce false;
    services.sshd.enable = lib.mkForce false;
  };
  microvm = {
    # Host supplies credentials through the read-only virtiofs share.
    hypervisor = "qemu";
    mem = 8192;
    vcpu = 4;
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

  # Split DNS selects Dewey's prim address, which this guest cannot reach.
  # Keep TLS hostnames while using svc for discovery and WOPI callbacks.
  networking.hosts.${deweyAddress} = ingressNames;
  virtualisation.quadlet.containers =
    lib.genAttrs
      [
        "opencloud-home"
        "opencloud-home-office"
      ]
      (container: {
        containerConfig.AddHost = map (name: "${name}:${deweyAddress}") ingressNames;
      });

  modules.services.opencloud.instances.home = {
    uid = 3100;
    gid = 3100;
    dataMount = "/mnt/opencloud";
    domain = "data.${home.homeDomain}";
    listenAddress = address;
    ports = {
      app = 9200;
      office = 9201;
    };
    environmentFile = "/run/opencloud-home/app.env";
    oidc = {
      issuer = "https://id.${home.homeDomain}";
      clientId = "opencloud-home";
    };
    environment = {
      OC_ADD_RUN_SERVICES = "collaboration,notifications";
      # Verify Office request signatures against its advertised WOPI proof keys.
      COLLABORATION_APP_PROOF_DISABLE = "false";
      PROXY_ROLE_ASSIGNMENT_DRIVER = "oidc";
      PROXY_ROLE_ASSIGNMENT_OIDC_CLAIM = "opencloud_home_roles";
      NOTIFICATIONS_SMTP_HOST = home.mail.host;
      NOTIFICATIONS_SMTP_PORT = toString home.mail.port;
      NOTIFICATIONS_SMTP_AUTHENTICATION = "none";
      NOTIFICATIONS_SMTP_ENCRYPTION = "none";
    };
    office = {
      domain = "euro-office.${home.homeDomain}";
      environmentFile = "/run/opencloud-home/office.env";
    };
  };
  fileSystems."/mnt/opencloud" = lib.mkIf (dataSource != null) {
    device = dataSource;
    fsType = "nfs";
    # A missing server must not block local state or the credential service.
    # The application preflight retries until this actual mount is available.
    options = [
      "vers=4.2"
      "noac"
      "hard"
      "nofail"
      "x-systemd.mount-timeout=30s"
    ];
  };
  networking.firewall.extraInputRules = ''
    ip saddr ${deweyAddress} tcp dport { 22, 9200, 9201 } accept
  '';
  services.openssh.listenAddresses = [
    {
      addr = address;
      port = 22;
    }
  ];

  systemd.services.opencloud-home-credentials = {
    wantedBy = [ "multi-user.target" ];
    after = [ "systemd-tmpfiles-setup.service" ];
    before = [ "user@3100.service" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      LoadCredential = [
        "IDM_ADMIN_PASSWORD:${config.modules.microvm-guest.hostSecrets.mountPoint}/IDM_ADMIN_PASSWORD"
        "JWT_SECRET:${config.modules.microvm-guest.hostSecrets.mountPoint}/JWT_SECRET"
      ];
    };
    script = ''
      ${pkgs.coreutils}/bin/install -d -m 0700 -o ${user} -g ${user} /run/opencloud-home
      ${pkgs.coreutils}/bin/install -m 0400 -o ${user} -g ${user} "$CREDENTIALS_DIRECTORY/IDM_ADMIN_PASSWORD" /run/opencloud-home/app.env
      ${pkgs.coreutils}/bin/install -m 0400 -o ${user} -g ${user} "$CREDENTIALS_DIRECTORY/JWT_SECRET" /run/opencloud-home/office.env
    '';
  };
  systemd.services."user@3100" = {
    requires = [ "opencloud-home-credentials.service" ];
    after = [ "opencloud-home-credentials.service" ];
  };
}
