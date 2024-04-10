{
  pkgs,
  config,
  lib,
  ...
}:
#
# This module is highly specific to my needs, so be careful using it.
# It exists so that I can enable/disable services easily across my various servers.
# The idea is if I want to move service foo to host A, I can just enable it with one flag and it will be deployed
# (of course I'd have to migrate the data, but that's easy enough with zfs send recv)
let
  home-ops = config.repo.secrets.home-ops;
  cfg = config.home-ops;
  nodeSettings = config.repo.secrets.global.nodes.${config.networking.hostName};
in
{
  options.home-ops = {
    enable = lib.mkEnableOption "My modular multi-host Home Ops setup";
    user = lib.mkOption {
      type = lib.types.attrs;
      description = "User config.";
    };
    postgresql = {
      enable = lib.mkEnableOption "Postgresql";
      onsiteBackup = {
        enable = lib.mkEnableOption "Onsite Backup";
        path = lib.mkOption {
          type = lib.types.str;
          default = "/${config.networking.hostName}/repo1";
        };
      };

      offsiteBackup = {
        enable = lib.mkEnableOption "Offsite Backup";
        path = lib.mkOption {
          type = lib.types.str;
          default = "/${config.networking.hostName}/repo2";
        };
      };
    };
    mariadb = {
      enable = lib.mkEnableOption "MariaDB";
    };
    hypervisor = {
      enable = lib.mkEnableOption "libvirt Hypervisor";
    };
    ingress = {
      enable = lib.mkEnableOption "NGINX Ingress";
    };
    containers = {
      enable = lib.mkEnableOption "OCI containers";
    };

    apps = {
      echo-server.enable = lib.mkEnableOption "Echo Server";
      davis.enable = lib.mkEnableOption "Davis, carddav and caldav server";
      invoiceninja.enable = lib.mkEnableOption "Invoice Ninja";
      authentik.enable = lib.mkEnableOption "Authentik";
      paperless.enable = lib.mkEnableOption "Paperless";
      ocis-work.enable = lib.mkEnableOption "oCIS Work";
      ocis-home.enable = lib.mkEnableOption "oCIS Home";
      plex.enable = lib.mkEnableOption "Plex";
      tautulli.enable = lib.mkEnableOption "Tautulli";
      home-dl.enable = lib.mkEnableOption "Home *arr";
      calibre.enable = lib.mkEnableOption "Calibre";
      calibre-web.enable = lib.mkEnableOption "Calibre Web";
      roon-server.enable = lib.mkEnableOption "Roon Server";
      onepassword-connect.enable = lib.mkEnableOption "1Password Connect";
      archivebox.enable = lib.mkEnableOption "Archivebox";
      linkding.enable = lib.mkEnableOption "Linkding";
    };
  };

  imports = [ ./zrepl.nix ];
  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion =
          cfg.postgresql.enable -> cfg.postgresql.onsiteBackup.enable || cfg.postgresql.offsiteBackup.enable;
        message = "Postgresql must be configured with backup repositories";
      }
      {
        assertion = !(cfg.apps.ocis-work.enable && cfg.apps.ocis-home.enable);
        message = "OCIS Work and OCIS Home cannot be enabled at the same time on the same host";
      }
    ];

    ###########
    ## Basic ##
    ###########
    time.timeZone = "Europe/Berlin";
    i18n.defaultLocale = "en_US.utf8";
    sops.age.sshKeyPaths = [ "/persist/etc/ssh/ssh_host_ed25519_key" ];
    documentation.nixos.enable = false;
    documentation.doc.enable = false;
    boot.kernel.sysctl = {
      "fs.inotify.max_queued_events" = 65536;
      "fs.inotify.max_user_watches" = 524288;
      "fs.inotify.max_user_instances" = 8192;
    };

    ############################
    ## My Custom Base Modules ##
    ############################
    modules = {
      shell = {
        htop.enable = true;
        tmux.enable = true;
        zsh.enable = true;
      };
      services = {
        sshd.enable = true;
      };
      editors = {
        vim.enable = true;
      };
      impermanence.enable = true;
      boot.zfs = {
        enable = true;
        encrypted = true;
        rootPool = "rpool";
        scrubPools = [ "rpool" ];
        extraPools = [ "tank" ];
        autoSnapshot.enable = false;
      };
      zfs.datasets.enable = true;
      server = {
        smtp-external-relay.enable = false;
      };
      # vpn.tailscale.enable = true;
      firewall.enable = true;
      security.default.enable = true;
      networking.default.enable = true;
      users.enable = true;
      users.primaryUser = {
        username = cfg.user.username;
        name = cfg.user.name;
        homeDirectory = cfg.user.homeDirectory;
        signingKey = cfg.user.signingKey;
        email = cfg.user.email;
        passwordSecretKey = cfg.user.passwordSecretKey;
        shell = pkgs.zsh;
        extraGroups = [
          "libvirtd"
          "wheel"
          "media"
        ];
      };
    };

    environment.systemPackages = with pkgs; [
      fd
      jq
      htop
      lshw
      ncdu
      python311
      rclone
      ripgrep
      smartmontools
      tcpdump
      vifm
      yq-go
      restic
    ];

    #
    # Supporting services
    #
    services.smartd.enable = true;
    services.rpcbind.enable = true;
    home-ops.zrepl.enable = true;
    services.prometheus = {
      exporters = {
        node = {
          enable = false;
          enabledCollectors = [ "systemd" ];
          disabledCollectors = [ "textfile" ];
          port = home-ops.ports.node-exporter;
        };
        zfs = {
          enable = true;
          port = home-ops.ports.zfs-exporter;
        };
        smartctl = {
          enable = false;
          port = home-ops.ports.smartctl-exporter;
        };
      };
    };
    networking.firewall.allowedTCPPorts = [
      config.services.prometheus.exporters.node.port
      config.services.prometheus.exporters.zfs.port
      config.services.prometheus.exporters.smartctl.port
    ];

    modules.server.virtd-host = lib.mkIf cfg.hypervisor.enable {
      enable = true;
      zfsStorage.enable = true;
    };
    sops.secrets.pgbackrestSecrets = lib.mkIf cfg.postgresql.enable {
      sopsFile = ../../configs/home-ops/shared.sops.yml;
      mode = "400";
    };
    modules.services.postgresql = lib.mkIf cfg.postgresql.enable {
      enable = true;
      package = pkgs.postgresql_15;
      secretsFile = config.sops.secrets.pgbackrestSecrets.path;
      repo1 = {
        enable = cfg.postgresql.onsiteBackup.enable;
        path = cfg.postgresql.onsiteBackup.path;
        bucket = home-ops.pgBackup.onsite.bucket;
        endpoint = home-ops.pgBackup.onsite.endpoint;
      };
      repo2 = {
        enable = cfg.postgresql.offsiteBackup.enable;
        path = cfg.postgresql.offsiteBackup.path;
        bucket = home-ops.pgBackup.offsite.bucket;
        endpoint = home-ops.pgBackup.offsite.endpoint;
      };
    };
    modules.services.mariadb = lib.mkIf cfg.mariadb.enable {
      enable = true;
      package = pkgs.mariadb_110;
    };
    modules.services.ingress = lib.mkIf cfg.ingress.enable {
      enable = true;
      domains = config.repo.secrets.local.domains;
    };

    virtualisation.podman.enable = cfg.containers.enable;
    virtualisation.oci-containers = lib.mkIf cfg.containers.enable { backend = "podman"; };

    ######################
    # Impermanence Setup #
    ######################
    environment.persistence."/persist" = {
      hideMounts = true;
      directories = [
        "/var/lib/nixos"
        "/var/lib/systemd/coredump"
      ];
      files = [ ];
    };

    systemd.tmpfiles.rules = [
      "d /persist/home/${cfg.user.username} 700 ${cfg.user.username} ${cfg.user.username}"
      "d /persist/home/${cfg.user.username}/.config 0775 ${cfg.user.username} ${cfg.user.username}  -"
      "d /persist/home/${cfg.user.username}/.local 755 ${cfg.user.username} ${cfg.user.username}"
      "d /persist/home/${cfg.user.username}/.local/state 755 ${cfg.user.username} ${cfg.user.username}"
      "d /persist/home/${cfg.user.username}/.local/state/zsh 755 ${cfg.user.username} ${cfg.user.username}"
    ];

    ################
    ## Networking ##
    ################
    networking.domain = home-ops.homeDomain;
    networking.usePredictableInterfaceNames = true;
    networking.firewall.allowPing = true;
    networking.nameservers = config.repo.secrets.global.nameservers;
    systemd.network = {
      netdevs = {
        "20-vlprim4" = {
          netdevConfig = {
            Kind = "vlan";
            Name = "vlprim4";
          };
          vlanConfig.Id = 4;
        };
        "20-vlmgmt9" = {
          netdevConfig = {
            Kind = "vlan";
            Name = "vlmgmt9";
          };
          vlanConfig.Id = 9;
        };
        "20-vldata11" = {
          netdevConfig = {
            Kind = "vlan";
            Name = "vldata11";
            MTUBytes = "9000";
          };
          vlanConfig.Id = 11;
        };
        "30-brprim4" = {
          netdevConfig = {
            Name = "brprim4";
            Kind = "bridge";
          };
        };
        "30-brmgmt9" = {
          netdevConfig = {
            Name = "brmgmt9";
            Kind = "bridge";
          };
        };
        "30-brdata11" = {
          netdevConfig = {
            Name = "brdata11";
            Kind = "bridge";
            MTUBytes = "9000";
          };
        };
      };

      networks = {
        "40-${nodeSettings.mgmtIface}" = {
          matchConfig.Name = "${nodeSettings.mgmtIface}";
          vlan = [
            "vlmgmt9"
            "vlprim4"
          ];
        };
        "40-${nodeSettings.dataIface}" = {
          matchConfig = {
            Name = "${nodeSettings.dataIface}";
          };
          networkConfig = {
            Description = "physical 10gbe";
          };
          linkConfig = {
            MTUBytes = "9000";
          };
          vlan = [ "vldata11" ];
        };
        "45-vlprim4" = {
          matchConfig = {
            Name = "vlprim4";
          };
          networkConfig = {
            Bridge = "brprim4";
          };
        };
        "45-vldata11" = {
          matchConfig = {
            Name = "vldata11";
          };
          networkConfig = {
            Bridge = "brdata11";
          };
        };
        "45-vlmgmt9" = {
          matchConfig = {
            Name = "vlmgmt9";
          };
          networkConfig = {
            Bridge = "brmgmt9";
          };
        };

        "50-brprim4" =
          if nodeSettings.vlanPrimaryEnabled then
            {
              matchConfig = {
                Name = "brprim4";
              };
              networkConfig = {
                DHCP = "no";
                Address = nodeSettings.primCIDR;
                Description = "primary VLAN";
              };
            }
          else
            {
              # My nodes generally do not have an ip address on the primary vlan
              # however some workloads running on these nodes might want to expose a service
              # over this vlan, so we configure the bridge interface anyways
              matchConfig = {
                Name = "brprim4";
              };
              networkConfig = {
                DHCP = "no";
                Description = "Bridge for primary vlan";
              };
            };
        "50-brmgmt9" = {
          matchConfig = {
            Name = "brmgmt9";
          };
          networkConfig = {
            DHCP = "no";
            Address = nodeSettings.mgmtCIDR;
            Gateway = config.repo.secrets.global.mgmtGateway;
            Description = "mgmt VLAN";
          };
        };
        "50-brdata11" = {
          matchConfig = {
            Name = "brdata11";
          };
          networkConfig = {
            DHCP = "no";
            Address = nodeSettings.dataCIDR;
            Description = "data 10GbE VLAN";
          };
        };
      };
    };

    ########################
    # Application Services #
    ########################

    # shared media user/group
    users.users.${home-ops.users.media.name} = {
      name = home-ops.users.media.name;
      uid = home-ops.users.media.uid;
      group = home-ops.groups.media.name;
      isSystemUser = true;
    };
    users.groups.${home-ops.groups.media.name} = {
      gid = home-ops.groups.media.gid;
    };
    modules.services.echo-server = lib.mkIf cfg.apps.echo-server.enable {
      enable = true;
      domain = "echo-test.${home-ops.homeDomain}";
      ports.http = home-ops.ports.echo-server;
      ingress = {
        external = true;
        domain = home-ops.homeDomain;
      };
    };

    modules.services.davis = lib.mkIf cfg.apps.davis.enable {
      enable = true;
      domain = "dav.${home-ops.homeDomain}";
      ingress = {
        external = true;
        domain = home-ops.homeDomain;
      };
    };

    modules.services.invoiceninja = lib.mkIf cfg.apps.invoiceninja.enable {
      enable = true;
      domain = "clients.${home-ops.workDomain}";
      ingress = {
        external = true;
        domain = home-ops.workDomain;
      };
    };

    modules.services.authentik = lib.mkIf cfg.apps.authentik.enable {
      enable = true;
      domain1 = "auth.${home-ops.homeDomain}";
      domain2 = "auth.${home-ops.workDomain}";
      ingress1 = home-ops.homeDomain;
      ingress2 = home-ops.workDomain;
      ports.http = home-ops.ports.authentik-http;
      ports.https = home-ops.ports.authentik-https;
    };

    modules.services.onepassword-connect = lib.mkIf cfg.apps.onepassword-connect.enable {
      enable = true;
      domain = "op.${home-ops.homeDomain}";
      ports.api = home-ops.ports.onepassword-connect-api;
      ports.sync = home-ops.ports.onepassword-connect-sync;
      user = home-ops.users.onepassword-connect;
      group = home-ops.groups.onepassword-connect;
      ingress = {
        domain = home-ops.homeDomain;
      };
    };

    modules.services.paperless = lib.mkIf cfg.apps.paperless.enable {
      enable = true;
      domain = "paperless.${home-ops.homeDomain}";
      ports.http = home-ops.ports.paperless-http;
      user = home-ops.users.paperless;
      group = home-ops.groups.paperless;
      nfsShare = "tank2/services/paperless";
      ingress = {
        domain = home-ops.homeDomain;
      };
    };

    modules.services.plex = lib.mkIf cfg.apps.plex.enable {
      enable = true;
      domain = "plex.${home-ops.homeDomain}";
      user = home-ops.users.plex;
      group = home-ops.groups.plex;
      nfsShare = "tank2/media";
      ingress = {
        domain = home-ops.homeDomain;
      };
    };

    modules.services.tautulli = lib.mkIf cfg.apps.tautulli.enable {
      enable = true;
      domain = "tautulli.${home-ops.homeDomain}";
      user = home-ops.users.tautulli;
      ports.http = home-ops.ports.tautulli-http;
      ingress = {
        domain = home-ops.homeDomain;
      };
    };

    modules.services.home-dl = lib.mkIf cfg.apps.home-dl.enable {
      enable = true;
      baseDomain = home-ops.homeDomain;
      ports = home-ops.ports.home-dl;
      mediaNfsShare = "tank2/media";
      subnet = home-ops.subnets.home-dl;
      ingress = {
        domain = home-ops.homeDomain;
        forwardAuth = true;
      };
    };

    modules.services.calibre = lib.mkIf cfg.apps.calibre.enable {
      enable = true;
      domain = "calibre.${home-ops.homeDomain}";
      ports.gui = home-ops.ports.calibre-gui;
      ports.server = home-ops.ports.calibre-server;
      mediaNfsShare = "tank2/media";
      ingress = {
        domain = home-ops.homeDomain;
      };
    };

    modules.services.calibre-web = lib.mkIf cfg.apps.calibre-web.enable {
      enable = true;
      domain = "books.${home-ops.homeDomain}";
      domainKobo = "books-kobo.${home-ops.homeDomain}";
      ports.http = home-ops.ports.calibre-web;
      mediaNfsShare = "tank2/media";
      user = home-ops.users.books;
      group = home-ops.groups.books;
      ingress = {
        domain = home-ops.homeDomain;
        external = true;
      };
    };

    modules.services.archivebox = lib.mkIf cfg.apps.archivebox.enable {
      enable = true;
      domain = "archive.${home-ops.homeDomain}";
      ports.http = home-ops.ports.archivebox;
      ingress = {
        domain = home-ops.homeDomain;
      };
    };

    modules.services.linkding = lib.mkIf cfg.apps.linkding.enable {
      enable = true;
      domain = "bookmarks.${home-ops.homeDomain}";
      ports.http = home-ops.ports.linkding;
      user = home-ops.users.linkding;
      group = home-ops.groups.linkding;
      ingress = {
        domain = home-ops.homeDomain;
      };
    };

    modules.services.roon-server = lib.mkIf cfg.apps.roon-server.enable { enable = true; };

    modules.services.ocis =
      if cfg.apps.ocis-work.enable then
        {
          enable = true;
          domain = "data.${home-ops.workDomain}";
          ports.http = home-ops.ports.ocis-http;
          user = home-ops.users.ocis-work;
          group = home-ops.groups.ocis-work;
          nfsShare = "tank2/services/work-ocis2";
          subnet = home-ops.subnets.ocis-work;
          ingress = {
            domain = home-ops.workDomain;
            external = true;
          };
        }
      else if cfg.apps.ocis-home.enable then
        {
          enable = true;
          domain = "drive.${home-ops.homeDomain}";
          ports.http = home-ops.ports.ocis-http;
          user = home-ops.users.ocis-home;
          group = home-ops.groups.ocis-home;
          nfsShare = "tank2/services/home-ocis2";
          subnet = home-ops.subnets.ocis-home;
          ingress = {
            domain = home-ops.homeDomain;
            external = true;
          };
        }
      else
        { };
  };
}
