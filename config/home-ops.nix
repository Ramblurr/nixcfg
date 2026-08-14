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
  inherit (config.modules.users.primaryUser) username;
  inherit (config.repo.secrets) home-ops;
  cfg = config.home-ops;
  nodeSettings = config.repo.secrets.global.nodes.${config.networking.hostName};
  jellyplexWatchedMappings = home-ops.jellyplexWatched.mappings;
  caddyIngressEnabled =
    cfg.ingress.enable && cfg.apps.calibre-web.enable && cfg.apps.calibre-web.caddySecurity.enable;
  homeDlUpstream =
    port: "http://${lib.my.cidrToIp config.modules.services.home-dl.subnet.nsAddr}:${toString port}";
  protectedCaddySecurityApplications =
    (lib.optionalAttrs cfg.apps.calibre.enable {
      "calibre-gui" = {
        publicHost = "calibre.${home-ops.homeDomain}";
        upstream = "http://127.0.0.1:${toString config.modules.services.calibre.ports.gui}";
      };
    })
    // (lib.optionalAttrs cfg.apps.filebrowser-quantum.enable {
      files = {
        publicHost = "files.${home-ops.homeDomain}";
        upstream = "http://127.0.0.1:${toString config.modules.services.filebrowser-quantum.ports.http}";
        identityHeaders.X-authentik-username = "userinfo|preferred_username";
      };
    })
    // (lib.optionalAttrs cfg.apps.home-dl.enable {
      prowlarr = {
        publicHost = "prowlarr.${home-ops.homeDomain}";
        upstream = homeDlUpstream 9696;
      };
      radarr = {
        publicHost = "radarr.${home-ops.homeDomain}";
        upstream = homeDlUpstream 7878;
      };
      sabnzbd = {
        publicHost = "sabnzbd.${home-ops.homeDomain}";
        upstream = homeDlUpstream 8080;
      };
      sonarr = {
        publicHost = "sonarr.${home-ops.homeDomain}";
        upstream = homeDlUpstream 8989;
      };
    })
    // (lib.optionalAttrs cfg.apps.tubearchivist.enable {
      tube = {
        publicHost = "tube.${home-ops.homeDomain}";
        upstream = "http://127.0.0.1:${toString config.modules.services.tubearchivist.port}";
      };
    });
  caddySecurityEnvPrefix = name: lib.toUpper (lib.replaceStrings [ "-" ] [ "_" ] name);
  caddySecurityEnvironmentLines =
    (lib.optionals cfg.apps.calibre-web.caddySecurity.enable [
      "CALIBRE_WEB_OIDC_CLIENT_SECRET=${config.sops.placeholder.calibre-web-oidc-client-secret}"
      "CALIBRE_WEB_SIGNING_KEY=${config.sops.placeholder.calibre-web-caddy-security-signing-key}"
    ])
    ++ lib.concatMap (name: [
      "${caddySecurityEnvPrefix name}_OIDC_CLIENT_SECRET=${
        config.sops.placeholder."${name}-oidc-client-secret"
      }"
      "${caddySecurityEnvPrefix name}_SIGNING_KEY=${
        config.sops.placeholder."${name}-caddy-security-signing-key"
      }"
    ]) (builtins.attrNames protectedCaddySecurityApplications);
  mkProtectedCaddySecurityApplication =
    name: application:
    application
    // {
      oidc = {
        issuerURL = "https://id.${home-ops.homeDomain}";
        clientID = name;
        clientSecretEnv = "${caddySecurityEnvPrefix name}_OIDC_CLIENT_SECRET";
        realm = "${name}-pocket-id";
      };
      signingKeyEnv = "${caddySecurityEnvPrefix name}_SIGNING_KEY";
      cookiePrefix = caddySecurityEnvPrefix name;
      requiredGroups = [ "admins" ];
    };
  podmanWaitForDns = pkgs.writeShellScript "podman-wait-for-dns" ''
    until ${pkgs.glibc.getent}/bin/getent ahostsv4 registry-1.docker.io >/dev/null 2>&1; do
      ${pkgs.coreutils}/bin/sleep 0.5
    done
  '';
in
{
  options.home-ops = {
    enable = lib.mkEnableOption "My modular multi-host Home Ops setup";
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
      davis.enable = lib.mkEnableOption "Davis, carddav and caldav server";
      invoiceninja.enable = lib.mkEnableOption "Invoice Ninja";
      authentik.enable = lib.mkEnableOption "Authentik";
      paperless.enable = lib.mkEnableOption "Paperless";
      ocis-work.enable = lib.mkEnableOption "oCIS Work";
      ocis-home.enable = lib.mkEnableOption "oCIS Home";
      plex.enable = lib.mkEnableOption "Plex";
      jellyfin.enable = lib.mkEnableOption "Jellyfin";
      jellyplex-watched = {
        enable = lib.mkEnableOption "JellyPlex-Watched";
        dryRun = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Log changes without marking shows or movies as played.";
        };
        interval = lib.mkOption {
          type = lib.types.ints.positive;
          default = 3600;
          description = "Seconds between JellyPlex-Watched sync passes.";
        };
      };
      tautulli.enable = lib.mkEnableOption "Tautulli";
      home-dl.enable = lib.mkEnableOption "Home *arr";
      hindsight.enable = lib.mkEnableOption "Hindsight agent memory";
      calibre.enable = lib.mkEnableOption "Calibre";
      koreader-sync.enable = lib.mkEnableOption "Koreader-Sync";
      calibre-web = {
        enable = lib.mkEnableOption "Calibre Web";
        caddySecurity = {
          enable = lib.mkEnableOption "Calibre Web through caddy-security";
          clientID = lib.mkOption {
            type = lib.types.nonEmptyStr;
            description = "Private Pocket ID client ID for Calibre Web";
          };
        };
      };
      roon-server.enable = lib.mkEnableOption "Roon Server";
      onepassword-connect.enable = lib.mkEnableOption "1Password Connect";
      archivebox.enable = lib.mkEnableOption "Archivebox";
      matrix-synapse.enable = lib.mkEnableOption "Matrix-Synapse";
      influxdb.enable = lib.mkEnableOption "Influxdb";
      git-archive.enable = lib.mkEnableOption "Git-Archive";
      forgejo.enable = lib.mkEnableOption "Forgejo";
      actual-server.enable = lib.mkEnableOption "Actual Budget Server";
      atuin-sync.enable = lib.mkEnableOption "Atuin Sync Server";
      soju.enable = lib.mkEnableOption "Soju IRC bouncer";
      snowflake-proxy.enable = lib.mkEnableOption "snowflake proxy";
      my-y2r.enable = lib.mkEnableOption "my-y2r";
      audiobookshelf.enable = lib.mkEnableOption "audiobookshelf";
      filebrowser-quantum.enable = lib.mkEnableOption "FileBrowser Quantum";
      tubearchivist.enable = lib.mkEnableOption "tubearchivist";
      stirling-pdf.enable = lib.mkEnableOption "Stirling PDF";
    };
  };

  imports = [ ./zrepl.nix ];
  config = lib.mkIf cfg.enable {
    assertions = [
      #{
      #  assertion =
      #    cfg.postgresql.enable -> cfg.postgresql.onsiteBackup.enable || cfg.postgresql.offsiteBackup.enable;
      #  message = "Postgresql must be configured with backup repositories";
      #}
      {
        assertion = !(cfg.apps.ocis-work.enable && cfg.apps.ocis-home.enable);
        message = "OCIS Work and OCIS Home cannot be enabled at the same time on the same host";
      }
      {
        assertion =
          !cfg.apps.jellyplex-watched.enable || (cfg.apps.plex.enable && cfg.apps.jellyfin.enable);
        message = "JellyPlex-Watched requires both Plex and Jellyfin to be enabled";
      }
      {
        assertion =
          !cfg.apps.calibre-web.caddySecurity.enable || (cfg.ingress.enable && cfg.apps.calibre-web.enable);
        message = "Calibre Web caddy-security requires ingress and Calibre Web";
      }
    ];

    ###########
    ## Basic ##
    ###########
    home.nix-lan-cache.enable = true;
    time.timeZone = "Europe/Berlin";
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
        usePlymouth = false;
      };
      zfs.datasets.enable = true;
      server = {
        smtp-external-relay.enable = true;
      };
      # vpn.tailscale.enable = true;
      firewall.enable = true;
      security.default.enable = true;
      users.enable = true;
      users.headless.enable = true;
      users.primaryUser.extraGroups = [
        "libvirtd"
        "audio"
        "wheel"
        "media"
      ];
    };

    environment.interactiveShellInit = ''
      # raise some awareness towards failed services
      systemctl --no-pager --failed || true
    '';

    environment.systemPackages = with pkgs; [
      bandwhich
      fd
      jq
      htop
      isd
      lshw
      ncdu
      python3
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
    services.rpcbind.enable = true;
    home-ops.zrepl.enable = true;

    modules.telemetry = {
      prometheus-zfs-exporter.enable = true;
      prometheus-smartctl-exporter.enable = true;
      smartd.enable = true;
      prometheus-node-exporter.enable = true;
    };

    networking.firewall.allowedUDPPorts = [
      443 # http3
      53 # dns
      67 # dhcp for microvms
    ];
    networking.firewall.allowedTCPPorts = [
      53
    ];

    modules.server.virtd-host = lib.mkIf cfg.hypervisor.enable {
      enable = true;
      storage.zfs.enable = true;
      net.prim.enable = true;
    };
    sops.secrets.pgbackrestSecrets = lib.mkIf cfg.postgresql.enable {
      sopsFile = ../configs/home-ops/shared.sops.yml;
      mode = "400";
    };
    modules.services.postgresql = lib.mkIf cfg.postgresql.enable {
      enable = true;
      package = pkgs.postgresql_15;
      secretsFile = config.sops.secrets.pgbackrestSecrets.path;
      repo1 = {
        inherit (cfg.postgresql.onsiteBackup) enable;
        inherit (cfg.postgresql.onsiteBackup) path;
        inherit (home-ops.pgBackup.onsite) bucket;
        inherit (home-ops.pgBackup.onsite) endpoint;
      };
      repo2 = {
        inherit (cfg.postgresql.offsiteBackup) enable;
        inherit (cfg.postgresql.offsiteBackup) path;
        inherit (home-ops.pgBackup.offsite) bucket;
        inherit (home-ops.pgBackup.offsite) endpoint;
      };
    };
    modules.services.mariadb = lib.mkIf cfg.mariadb.enable {
      enable = true;
      package = pkgs.mariadb_114;
    };
    modules.services.ingress = lib.mkIf cfg.ingress.enable {
      enable = true;
      inherit (config.repo.secrets.local) domains;
      virtualHosts."home.${home-ops.homeDomain}" = lib.mkIf (!caddyIngressEnabled) {
        upstream = "http://10.9.4.25:8123";
        acmeHost = home-ops.homeDomain;
      };
      virtualHosts."octoprint.${home-ops.homeDomain}" = lib.mkIf (!caddyIngressEnabled) {
        upstream = "http://10.8.50.52:5000";
        acmeHost = home-ops.homeDomain;
      };
    };
    modules.services.caddy.routes = lib.mkIf caddyIngressEnabled (
      (lib.optionalAttrs cfg.apps.atuin-sync.enable {
        atuin = {
          publicHost = "atuin.${home-ops.homeDomain}";
          upstream = "http://127.0.0.1:${toString config.modules.services.atuin-sync.ports.http}";
          requestBodyMaxSize = "10MB";
        };
      })
      // (lib.optionalAttrs cfg.apps.audiobookshelf.enable {
        audiobookshelf = {
          publicHost = "audiobookshelf.${home-ops.homeDomain}";
          upstream = "http://127.0.0.1:${toString config.modules.services.audiobookshelf.ports.http}";
        };
      })
      // (lib.optionalAttrs config.modules.services.ingress-nixbot.enable {
        ci = {
          publicHost = "ci.${home-ops.workDomain}";
          upstream = "http://debord.prim.${home-ops.homeDomain}:${toString home-ops.ports.nixbot}";
          requestBodyMaxSize = "25MB";
          responseHeaders.X-Robots-Tag = "noindex, nofollow, noarchive";
          dialTimeout = "120s";
          flushInterval = "-1";
          staticResponses."/robots.txt" = {
            body = "User-agent: *\nDisallow: /\n";
            headers.X-Robots-Tag = "noindex, nofollow, noarchive";
          };
        };
      })
      // (lib.optionalAttrs cfg.apps.invoiceninja.enable {
        clients = {
          publicHost = "clients.${home-ops.workDomain}";
          upstream = "http://127.0.0.1:${toString config.modules.services.invoiceninja.ports.http}";
        };
      })
      // (lib.optionalAttrs cfg.apps.ocis-work.enable {
        data = {
          publicHost = "data.${home-ops.workDomain}";
          upstream = "http://${lib.my.cidrToIp config.modules.services.ocis.subnet.nsAddr}:${toString config.modules.services.ocis.ports.http}";
        };
      })
      // (lib.optionalAttrs cfg.apps.forgejo.enable {
        forgejo = {
          publicHost = "git.${home-ops.homeDomain}";
          upstream = "unix/${config.services.forgejo.settings.server.HTTP_ADDR}";
          requestBodyMaxSize = "10MB";
        };
      })
      // (lib.optionalAttrs cfg.apps.influxdb.enable {
        influxdb = {
          publicHost = "influxdb.${home-ops.homeDomain}";
          upstream = "http://127.0.0.1:${toString config.modules.services.influxdb.ports.http}";
        };
      })
      // (lib.optionalAttrs cfg.apps.jellyfin.enable {
        jellyfin = {
          publicHost = "jelly.${home-ops.homeDomain}";
          upstream = "http://127.0.0.1:8096";
          requestBodyMaxSize = "10MB";
          flushInterval = "-1";
          directWan = true;
        };
      })
      // (lib.optionalAttrs cfg.apps.paperless.enable {
        paperless = {
          publicHost = "paperless.${home-ops.homeDomain}";
          upstream = "http://127.0.0.1:${toString config.modules.services.paperless.ports.http}";
        };
      })
      // (lib.optionalAttrs config.modules.services.ingress-paseo.enable {
        paseo = {
          publicHost = "paseo.${home-ops.homeDomain}";
          upstream = "http://quine.prim.${home-ops.homeDomain}:6767";
          requestBodyMaxSize = "100MB";
          requestHeaders = {
            Host = "{http.request.host}";
            X-Forwarded-For = "{http.request.header.X-Forwarded-For}";
            X-Forwarded-Proto = "https";
          };
          dialTimeout = "120s";
          flushInterval = "-1";
        };
      })
      // (lib.optionalAttrs cfg.apps.stirling-pdf.enable {
        pdf = {
          publicHost = "pdf.${home-ops.homeDomain}";
          upstream = "http://127.0.0.1:${toString config.modules.services.stirling-pdf.ports.http}";
          requestBodyMaxSize = "10MB";
        };
      })
      // (lib.optionalAttrs cfg.apps.home-dl.enable {
        qbittorrent = {
          publicHost = "qbittorrent.${home-ops.homeDomain}";
          upstream = "http://127.0.0.1:${toString config.modules.services.home-dl.ports.qbittorrent}";
          requestBodyMaxSize = "10MB";
        };
      })
      // (lib.optionalAttrs cfg.apps.authentik.enable {
        authentik-home = {
          publicHost = "auth.${home-ops.homeDomain}";
          handlerConfig = ''
            reverse_proxy https://127.0.0.1:${toString config.modules.services.authentik.ports.https} {
              transport http {
                tls_insecure_skip_verify
              }
            }
          '';
        };
        authentik-work = {
          publicHost = "auth.${home-ops.workDomain}";
          handlerConfig = ''
            reverse_proxy https://127.0.0.1:${toString config.modules.services.authentik.ports.https} {
              transport http {
                tls_insecure_skip_verify
              }
            }
          '';
        };
      })
      // (lib.optionalAttrs cfg.apps.calibre-web.enable {
        books-kobo = {
          publicHost = "books-kobo.${home-ops.homeDomain}";
          upstream = "http://127.0.0.1:${toString config.modules.services.calibre-web.ports.http}";
          requestHeaders.X-Scheme = "https";
        };
      })
      // (lib.optionalAttrs cfg.apps.calibre.enable {
        calibre-server = {
          publicHost = "calibre-server.${home-ops.homeDomain}";
          upstream = "http://127.0.0.1:${toString config.modules.services.calibre.ports.server}";
        };
      })
      // (lib.optionalAttrs cfg.apps.davis.enable {
        davis = {
          publicHost = "dav.${home-ops.homeDomain}";
          handlerConfig = ''
            @davis_well_known path /.well-known/caldav /.well-known/carddav
            redir @davis_well_known https://{http.request.host}/dav/ 302
            @davis_hidden path_regexp davis_hidden /\.ht
            respond @davis_hidden 404
            root * ${config.services.davis.package}/public
            php_fastcgi unix//run/phpfpm/davis.sock {
              env HTTPS on
              env HTTP_X_FORWARDED_PROTO https
              env HTTP_X_FORWARDED_PORT 443
            }
            file_server
          '';
        };
      })
      // {
        home-assistant = {
          publicHost = "home.${home-ops.homeDomain}";
          upstream = "http://10.9.4.25:8123";
        };
        octoprint = {
          publicHost = "octoprint.${home-ops.homeDomain}";
          handlerConfig = ''
            handle_path /webcam/* {
              reverse_proxy 10.8.50.52:8080 {
                flush_interval -1
                transport http {
                  read_timeout 24h
                }
              }
            }
            handle {
              reverse_proxy 10.8.50.52:5000
            }
          '';
        };
      }
      // (lib.optionalAttrs cfg.apps.koreader-sync.enable {
        koreader = {
          publicHost = "koreader.${home-ops.homeDomain}";
          upstream = "http://127.0.0.1:${toString config.modules.services.koreader-sync.ports.http}";
          requestBodyMaxSize = "10MB";
        };
      })
      // (lib.optionalAttrs cfg.apps.matrix-synapse.enable {
        matrix = {
          publicHost = "matrix.${home-ops.workDomain}";
          http3 = false;
          handlerConfig = ''
            @matrix_server path /.well-known/matrix/server
            handle @matrix_server {
              header Content-Type application/json
              respond ${
                builtins.toJSON (builtins.toJSON { "m.server" = "matrix.${home-ops.workDomain}:443"; })
              } 200
            }
            handle /admin {
              redir * /admin/ 307
            }
            handle_path /admin/* {
              @matrix_admin_assets path_regexp matrix_admin_assets \.(?:css|js|jpg|jpeg|gif|png|svg|ico|woff|woff2|ttf|eot|webp)$
              header @matrix_admin_assets Cache-Control "public, max-age=2592000"
              root * ${config.modules.services.matrix-synapse.ketesa.package}
              try_files {path} {path}/ /index.html
              file_server
            }
            @matrix_api path /_matrix/* /_synapse/client/*
            handle @matrix_api {
              request_body {
                max_size 200MB
              }
              reverse_proxy 127.0.0.1:${toString config.modules.services.matrix-synapse.ports.http}
            }
            handle {
              reverse_proxy 127.0.0.1:${toString config.modules.services.matrix-synapse.ports.http}
            }
          '';
        };
      })
      // (lib.optionalAttrs config.modules.services.ingress-phoniebox.enable {
        phoniebox = {
          publicHost = "phoniebox.${home-ops.homeDomain}";
          handlerConfig = ''
            handle_path /.fairybox-offline/* {
              header Cache-Control "public, max-age=3600"
              root * ${../modules/services/ingress-phoniebox}
              file_server
            }
            route {
              intercept {
                @phoniebox_offline status 502 503 504
                handle_response @phoniebox_offline {
                  rewrite * /index.html
                  root * ${../modules/services/ingress-phoniebox}
                  file_server {
                    status 503
                  }
                }
              }
              reverse_proxy 10.9.6.26:80 {
                flush_interval -1
                transport http {
                  dial_timeout 3s
                  response_header_timeout 1h
                }
              }
            }
          '';
          errorHandlerConfig = ''
            @phoniebox_proxy_error {
              host phoniebox.${home-ops.homeDomain}
              expression {http.error.status_code} in [502, 503, 504]
            }
            handle @phoniebox_proxy_error {
              rewrite * /index.html
              root * ${../modules/services/ingress-phoniebox}
              file_server {
                status 503
              }
            }
          '';
        };
      })
      // (lib.optionalAttrs cfg.apps.my-y2r.enable {
        y2pod = {
          publicHost = "y2pod.${home-ops.homeDomain}";
          handlerConfig = ''
            @y2pod_private path_regexp y2pod_private (^|/)\.
            respond @y2pod_private 403
            @y2pod_logs path_regexp y2pod_logs \.log$
            respond @y2pod_logs 403
            @y2pod_work path_regexp y2pod_work ^/[^/]+/(inbox|processing|archive)(/|$)
            respond @y2pod_work 403
            @y2pod_rss path_regexp y2pod_rss \.rss$
            header @y2pod_rss Content-Type "application/rss+xml; charset=utf-8"
            header Accept-Ranges bytes
            root * /var/lib/y2r
            file_server browse
          '';
        };
      })
    );

    virtualisation.podman.enable = lib.mkIf cfg.containers.enable true;
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
      users.${username}.directories = [ ".config/sops" ];
    };

    systemd.tmpfiles.rules = [
      "d /persist/home/${username} 700 ${username} ${username}"
      "d /persist/home/${username}/.config 0775 ${username} ${username}  -"
      "d /persist/home/${username}/.local 755 ${username} ${username}"
      "d /persist/home/${username}/.local/state 755 ${username} ${username}"
      "d /persist/home/${username}/.local/state/zsh 755 ${username} ${username}"
    ];

    ################
    ## Networking ##
    ################
    networking.usePredictableInterfaceNames = true;
    networking.firewall.allowPing = true;
    networking.nameservers = [ "127.0.0.1" ];
    services.resolved.enable = lib.mkForce false;
    environment.etc."resolv-external.conf" = {
      mode = "0644";
      text = ''
        nameserver ${lib.my.cidrToIp nodeSettings.mgmtCIDR}
      '';
    };
    services.dnsdist = {
      enable = true;
      extraConfig =
        let
          transformDomainToRegex =
            domain:
            let
              escapedDomain = lib.replaceStrings [ "." ] [ "\\\\." ] domain;
            in
            "(^|\\\\.)${escapedDomain}$";
        in
        ''
          -- disable security status polling via DNS
          setSecurityPollSuffix("")

          -- udp/tcp dns listening
          setLocal("127.0.0.1:53", {})
          addLocal("${lib.my.cidrToIp nodeSettings.mgmtCIDR}:53", {})
          addLocal("${lib.my.cidrToIp nodeSettings.primCIDR}:53", {})

          -- Local LAN
          newServer({
            address = "${lib.elemAt config.repo.secrets.global.nameservers 0}",
            pool = "local",
            healthCheckMode = "lazy",
            checkInterval = 60,
            maxCheckFailures = 3,
            lazyHealthCheckFailedInterval = 30,
            rise = 2,
            lazyHealthCheckThreshold = 30,
            lazyHealthCheckSampleSize = 100,
            lazyHealthCheckMinSampleCount = 10,
            lazyHealthCheckMode = 'TimeoutOnly',
            useClientSubnet = true
          })
          newServer({
            address = "${lib.elemAt config.repo.secrets.global.nameservers 1}",
            pool = "local",
            healthCheckMode = "lazy",
            checkInterval = 60,
            maxCheckFailures = 3,
            lazyHealthCheckFailedInterval = 30,
            rise = 2,
            lazyHealthCheckThreshold = 30,
            lazyHealthCheckSampleSize = 100,
            lazyHealthCheckMinSampleCount = 10,
            lazyHealthCheckMode = 'TimeoutOnly',
            useClientSubnet = true
          })

          -- CloudFlare DNS over TLS
          newServer({
            address = "1.1.1.1:853",
            tls = "openssl",
            subjectName = "cloudflare-dns.com",
            validateCertificates = true,
            checkInterval = 60,
            checkTimeout = 2000,
            pool = "cloudflare"
          })
          newServer({
            address = "1.0.0.1:853",
            tls = "openssl",
            subjectName = "cloudflare-dns.com",
            validateCertificates = true,
            checkInterval = 60,
            checkTimeout = 2000,
            pool = "cloudflare"
          })

          -- Enable caching
          pc = newPacketCache(500000, {
            maxTTL = 3600,
            minTTL = 0,
            temporaryFailureTTL = 60,
            staleTTL = 60,
            dontAge = false
          })
          getPool(""):setCache(pc)


          -- Request logging, uncomment to log DNS requests/responses to stdout
          --addAction(AllRule(), LogAction("", false, false, true, false, false))
          --addResponseAction(AllRule(), LogResponseAction("", false, true, false, false))

          -- Routing rules

          addAction(RegexRule('${transformDomainToRegex home-ops.homeDomain}'), PoolAction('local'))
          addAction(RegexRule('${transformDomainToRegex home-ops.workDomain}'), PoolAction('local'))
          addAction('1.10.in-addr.arpa', PoolAction('local'))
          addAction(AllRule(), PoolAction("cloudflare"))
        '';
    };

    # dnsdist is the sole DNS resolver on hosts where systemd-resolved is
    # disabled. If it is stopped during nixos-rebuild switch activation and
    # not restarted (because its unit file did not change), all DNS lookups
    # fail until it is manually restarted. Always-restart prevents that window.
    systemd.services.dnsdist.serviceConfig = {
      Restart = "always";
      RestartSec = "5s";
    };

    # Podman's rootless Quadlets already wait for this user unit to observe the
    # system network-online target. On these hosts, DNS is provided by the local
    # dnsdist instance and can remain unavailable while its upstream health
    # checks recover. Keep the unit activating until external DNS works so
    # containers do not exhaust their image-pull retries during boot.
    systemd.user.services.podman-user-wait-network-online =
      lib.mkIf config.virtualisation.podman.enable
        {
          serviceConfig = {
            ExecStartPost = podmanWaitForDns;
            TimeoutStartSec = "180s";
          };
        };

    ########################
    # Application Services #
    ########################

    # shared media user/group
    users.users.${home-ops.users.media.name} = {
      inherit (home-ops.users.media) name;
      inherit (home-ops.users.media) uid;
      group = home-ops.groups.media.name;
      isSystemUser = true;
    };
    users.groups.${home-ops.groups.media.name} = {
      inherit (home-ops.groups.media) gid;
    };

    sops.secrets.calibre-web-oidc-client-secret = lib.mkIf caddyIngressEnabled {
      sopsFile = ../configs/home-ops/shared.sops.yml;
    };
    sops.secrets.calibre-web-caddy-security-signing-key = lib.mkIf caddyIngressEnabled {
      sopsFile = ../configs/home-ops/shared.sops.yml;
    };
    sops.secrets.calibre-gui-oidc-client-secret = lib.mkIf cfg.apps.calibre.enable {
      sopsFile = ../configs/home-ops/shared.sops.yml;
    };
    sops.secrets.calibre-gui-caddy-security-signing-key = lib.mkIf cfg.apps.calibre.enable {
      sopsFile = ../configs/home-ops/shared.sops.yml;
    };
    sops.secrets.files-oidc-client-secret = lib.mkIf cfg.apps.filebrowser-quantum.enable {
      sopsFile = ../configs/home-ops/shared.sops.yml;
    };
    sops.secrets.files-caddy-security-signing-key = lib.mkIf cfg.apps.filebrowser-quantum.enable {
      sopsFile = ../configs/home-ops/shared.sops.yml;
    };
    sops.secrets.prowlarr-oidc-client-secret = lib.mkIf cfg.apps.home-dl.enable {
      sopsFile = ../configs/home-ops/shared.sops.yml;
    };
    sops.secrets.prowlarr-caddy-security-signing-key = lib.mkIf cfg.apps.home-dl.enable {
      sopsFile = ../configs/home-ops/shared.sops.yml;
    };
    sops.secrets.radarr-oidc-client-secret = lib.mkIf cfg.apps.home-dl.enable {
      sopsFile = ../configs/home-ops/shared.sops.yml;
    };
    sops.secrets.radarr-caddy-security-signing-key = lib.mkIf cfg.apps.home-dl.enable {
      sopsFile = ../configs/home-ops/shared.sops.yml;
    };
    sops.secrets.sabnzbd-oidc-client-secret = lib.mkIf cfg.apps.home-dl.enable {
      sopsFile = ../configs/home-ops/shared.sops.yml;
    };
    sops.secrets.sabnzbd-caddy-security-signing-key = lib.mkIf cfg.apps.home-dl.enable {
      sopsFile = ../configs/home-ops/shared.sops.yml;
    };
    sops.secrets.sonarr-oidc-client-secret = lib.mkIf cfg.apps.home-dl.enable {
      sopsFile = ../configs/home-ops/shared.sops.yml;
    };
    sops.secrets.sonarr-caddy-security-signing-key = lib.mkIf cfg.apps.home-dl.enable {
      sopsFile = ../configs/home-ops/shared.sops.yml;
    };
    sops.secrets.tube-oidc-client-secret = lib.mkIf cfg.apps.tubearchivist.enable {
      sopsFile = ../configs/home-ops/shared.sops.yml;
    };
    sops.secrets.tube-caddy-security-signing-key = lib.mkIf cfg.apps.tubearchivist.enable {
      sopsFile = ../configs/home-ops/shared.sops.yml;
    };
    sops.templates.caddy-security-env = lib.mkIf caddyIngressEnabled {
      owner = "caddy";
      group = "caddy";
      mode = "0400";
      restartUnits = [ "caddy.service" ];
      content = lib.concatStringsSep "\n" caddySecurityEnvironmentLines;
    };

    # Expected SOPS key: jellyplex-watched.env with PLEX_TOKEN and JELLYFIN_TOKEN.
    sops.secrets."jellyplex-watched/env" = lib.mkIf cfg.apps.jellyplex-watched.enable {
      sopsFile = ../configs/home-ops/shared.sops.yml;
      mode = "400";
    };
    systemd.services.jellyplex-watched = lib.mkIf cfg.apps.jellyplex-watched.enable {
      wants = [ "sops-install-secrets.service" ];
      after = [ "sops-install-secrets.service" ];
    };
    modules.services.git-archive = lib.mkIf cfg.apps.git-archive.enable { enable = true; };

    modules.services.davis = lib.mkIf cfg.apps.davis.enable {
      enable = true;
      domain = "dav.${home-ops.homeDomain}";
      ingress = {
        external = true;
        domain = home-ops.homeDomain;
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

    modules.services.invoiceninja = lib.mkIf cfg.apps.invoiceninja.enable {
      enable = true;
      domain = "clients.${home-ops.workDomain}";
      user = home-ops.users.invoiceninja2;
      group = home-ops.groups.invoiceninja2;
      ports.http = home-ops.ports.invoiceninja;
      subnet = home-ops.subnets.invoiceninja2;
      ingress = {
        external = true;
        domain = home-ops.workDomain;
      };
    };

    #modules.services.onepassword-connect = lib.mkIf cfg.apps.onepassword-connect.enable {
    #  enable = true;
    #  domain = "op.${home-ops.homeDomain}";
    #  ports.api = home-ops.ports.onepassword-connect-api;
    #  ports.sync = home-ops.ports.onepassword-connect-sync;
    #  user = home-ops.users.onepassword-connect;
    #  group = home-ops.groups.onepassword-connect;
    #  ingress = {
    #    domain = home-ops.homeDomain;
    #  };
    #};

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

    modules.services.jellyfin = lib.mkIf cfg.apps.jellyfin.enable {
      enable = true;
      domain = "jelly.${home-ops.homeDomain}";
      user = home-ops.users.jellyfin;
      group = home-ops.groups.jellyfin;
      nfsShare = "tank2/media";
      ingress = {
        domain = home-ops.homeDomain;
        forwardAuth = false;
        directWan = true;
      };
      "jellyplex-watched" = lib.mkIf cfg.apps.jellyplex-watched.enable {
        enable = true;
        environmentFile = config.sops.secrets."jellyplex-watched/env".path;
        dryRun = cfg.apps.jellyplex-watched.dryRun;
        interval = cfg.apps.jellyplex-watched.interval;
        mappings = {
          users = jellyplexWatchedMappings.users or { };
          libraries = jellyplexWatchedMappings.libraries or { };
        };
      };
    };

    modules.services.audiobookshelf = lib.mkIf cfg.apps.audiobookshelf.enable {
      enable = true;
      domain = "audiobookshelf.${home-ops.homeDomain}";
      user = home-ops.users.audiobookshelf;
      group = home-ops.groups.audiobookshelf;
      nfsShare = "tank2/media";
      ports.http = home-ops.ports.audiobookshelf;
      ingress = {
        domain = home-ops.homeDomain;
      };
    };

    modules.services.filebrowser-quantum = lib.mkIf cfg.apps.filebrowser-quantum.enable {
      enable = true;
      domain = "files.${home-ops.homeDomain}";
      user = home-ops.users.media;
      group = home-ops.groups.media;
      ports.http = home-ops.ports.filebrowser-quantum;
      sources = [
        {
          name = "Downloads";
          path = "/mnt/downloads";
        }
        {
          name = "Media";
          path = "/mnt/mali/tank2/media";
        }
      ];
      ingress = {
        domain = home-ops.homeDomain;
        forwardAuth = true;
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

    modules.services.hindsight = lib.mkIf cfg.apps.hindsight.enable {
      enable = true;
      domain = "hindsight.${home-ops.homeDomain}";
      acmeHost = home-ops.homeDomain;
    };

    modules.services.calibre = lib.mkIf cfg.apps.calibre.enable {
      enable = true;
      domain.gui = "calibre.${home-ops.homeDomain}";
      domain.server = "calibre-server.${home-ops.homeDomain}";
      ports.gui = home-ops.ports.calibre-gui;
      ports.server = home-ops.ports.calibre-server;
      mediaNfsShare = "tank2/media";
      ingress = {
        domain = home-ops.homeDomain;
      };
    };

    modules.services.koreader-sync = lib.mkIf cfg.apps.koreader-sync.enable {
      enable = true;
      domain = "koreader.${home-ops.homeDomain}";
      ports.http = home-ops.ports.koreader-sync;
      ingress = {
        domain = home-ops.homeDomain;
        external = true;
      };
      user = home-ops.users.koreader-sync;
      group = home-ops.groups.koreader-sync;
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
    modules.services.caddy-security =
      lib.mkIf (cfg.apps.calibre-web.enable && cfg.apps.calibre-web.caddySecurity.enable)
        {
          enable = true;
          environmentFile = config.sops.templates.caddy-security-env.path;
          applications = {
            calibre-web = {
              publicHost = "books.${home-ops.homeDomain}";
              upstream = "127.0.0.1:${toString home-ops.ports.calibre-web}";
              portalPath = "/auth";
              oidc = {
                issuerURL = "https://id.${home-ops.homeDomain}";
                clientID = cfg.apps.calibre-web.caddySecurity.clientID;
                clientSecretEnv = "CALIBRE_WEB_OIDC_CLIENT_SECRET";
                realm = "calibre-pocket-id";
              };
              signingKeyEnv = "CALIBRE_WEB_SIGNING_KEY";
              cookiePrefix = "CALIBRE_WEB";
              requiredGroups = [ "books" ];
              bypassPathPrefixes = [ "/opds" ];
              identityHeaders = {
                Remote-User = "userinfo|preferred_username";
                Remote-Name = "userinfo|preferred_username";
                Remote-Email = "email";
                Remote-Groups = "roles";
                X-Auth-Request-User = "sub";
                X-Auth-Request-Preferred-Username = "userinfo|preferred_username";
                X-Auth-Request-Email = "email";
                X-Auth-Request-Groups = "roles";
                X-authentik-username = "userinfo|preferred_username";
                X_authentik_username = "userinfo|preferred_username";
                X-authentik-groups = "roles";
                X-authentik-email = "email";
                X-authentik-name = "userinfo|preferred_username";
                X-authentik-uid = "sub";
              };
            };
          }
          // lib.mapAttrs mkProtectedCaddySecurityApplication protectedCaddySecurityApplications;
        };

    #modules.services.archivebox = lib.mkIf cfg.apps.archivebox.enable {
    #  enable = true;
    #  domain = "archive.${home-ops.homeDomain}";
    #  ports.http = home-ops.ports.archivebox;
    #  user = home-ops.users.archivebox;
    #  group = home-ops.groups.archivebox;
    #  ingress = {
    #    domain = home-ops.homeDomain;
    #  };
    #};

    modules.services.influxdb = lib.mkIf cfg.apps.influxdb.enable {
      enable = true;
      domain = "influxdb.${home-ops.homeDomain}";
      ports.http = home-ops.ports.influxdb;
      ingress = {
        domain = home-ops.homeDomain;
      };
    };

    modules.services.matrix-synapse = lib.mkIf cfg.apps.matrix-synapse.enable {
      enable = true;
      domain = "matrix.${home-ops.workDomain}";
      serverName = home-ops.workDomain;
      ports.http = home-ops.ports.matrix-synapse;
      user = home-ops.users.matrix-synapse;
      group = home-ops.groups.matrix-synapse;
      bridgesGroup = home-ops.groups.matrix-bridges;
      ingress = {
        domain = home-ops.workDomain;
        external = true;
      };
      ketesa.enable = true;
      bridges.discord = {
        enable = true;
        user = home-ops.users.mautrix-discord;
        group = home-ops.groups.mautrix-discord;
        ports.http = home-ops.ports.mautrix-discord;
      };
      bridges.irc = {
        enable = true;
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
          cspYaml = home-ops.ocis-work-csp;
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

    modules.services.forgejo = lib.mkIf cfg.apps.forgejo.enable {
      enable = true;
      domain = "git.${home-ops.homeDomain}";
      user = home-ops.users.forgejo;
      group = home-ops.groups.forgejo;
      ingress = {
        domain = home-ops.homeDomain;
      };
    };

    modules.services.actual-server = lib.mkIf cfg.apps.actual-server.enable {
      enable = true;
      domain = "budget.${home-ops.homeDomain}";
      ports.http = home-ops.ports.actual-server;
      ingress = {
        domain = home-ops.homeDomain;
      };
    };
    modules.services.atuin-sync = lib.mkIf cfg.apps.atuin-sync.enable {
      enable = true;
      domain = "atuin.${home-ops.homeDomain}";
      ports.http = home-ops.ports.atuin-sync;
      ingress = {
        domain = home-ops.homeDomain;
      };
    };
    modules.services.soju = lib.mkIf cfg.apps.soju.enable {
      enable = true;
      domain = "irc.${home-ops.homeDomain}";
      ports.irc = home-ops.ports.soju-irc;
    };
    services.snowflake-proxy = lib.mkIf cfg.apps.snowflake-proxy.enable {
      enable = true;
      capacity = 50;
    };
    modules.services.my-y2r = lib.mkIf cfg.apps.my-y2r.enable {
      enable = true;
      domain = "y2pod.${home-ops.homeDomain}";
      ingress = {
        domain = home-ops.homeDomain;
        external = true;
      };
    };

    modules.services.stirling-pdf = lib.mkIf cfg.apps.stirling-pdf.enable {
      enable = true;
      domain = "pdf.${home-ops.homeDomain}";
      ports.http = home-ops.ports.stirling-pdf;
      ingress = {
        domain = home-ops.homeDomain;
      };
    };

    modules.services.tubearchivist = lib.mkIf cfg.apps.tubearchivist.enable {
      enable = true;
      domain = "tube.${home-ops.homeDomain}";
      port = home-ops.ports.tubearchivist;
      mediaNfsShare = "tank2/media/youtube";
      user = home-ops.users.tubearchivist;
      group = home-ops.groups.tubearchivist;
      ingress = {
        domain = home-ops.homeDomain;
      };
    };
  };
}
