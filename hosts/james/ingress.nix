{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.hosts.james.ingress;
  routes = import ./ingress-routes.nix { inherit config; };
  inherit (config.repo.secrets.global.ingressTunnel)
    jamesLocal
    deweyLocal
    thingsteadLocal
    clientPort
    ;

  gostConfig = pkgs.writeText "gost.json" (
    builtins.toJSON {
      log = {
        #level = "debug";
        format = "text";
      };
      services = [
        {
          name = "james-listen-new";
          addr = ":3435";
          handler = {
            type = "tunnel";
            auther = "auther-0";
            metadata = {
              "entrypoint.id" = "493004fa-5f6a-4363-9f12-4882392aebac";
              ingress = "james-ingress";
            };
          };
          listener = {
            type = "tcp";
          };
        }
        {
          name = "james-listen";
          addr = ":${toString clientPort}";
          handler = {
            type = "tunnel";
            auther = "auther-0";
            metadata = {
              entrypoint = ":443";
              ingress = "james-ingress";
            };
          };
          listener = {
            type = "tcp";
          };
        }
      ];
      authers = [
        {
          name = "auther-0";
          file = {
            path = "$GOST_AUTH_FILE";
          };
        }
      ];
      ingresses = [
        {
          name = "james-ingress";
          rules =
            (map (hostname: {
              inherit hostname;
              endpoint = deweyLocal;
            }) routes.deweyServices)
            ++ (map (hostname: {
              inherit hostname;
              endpoint = thingsteadLocal;
            }) routes.thingsteadServices)
            ++ (map (hostname: {
              inherit hostname;
              endpoint = jamesLocal;
            }) routes.localServices);
        }
      ];
    }
  );
  gostClientConfig = pkgs.writeText "gost.json" (
    builtins.toJSON {
      log = {
        level = "debug";
        format = "text";
      };
      services = [
        {
          name = "james-local-tcp";
          addr = ":0";
          handler = {
            type = "rtcp";
            metadata = {
              proxyProtocol = "1";
            };
          };
          listener = {
            type = "rtcp";
            chain = "chain-0";
          };
          forwarder = {
            nodes = [
              {
                name = "local-nginx";
                addr = "127.0.0.1:8443";
              }
            ];
          };
        }
        {
          name = "thingstead-local-tcp";
          addr = ":0";
          handler = {
            type = "rtcp";
          };
          listener = {
            type = "rtcp";
            chain = "chain-thingstead";
          };
          forwarder = {
            nodes = [
              {
                name = "thingstead";
                addr = "${routes.haproxyBackends.thingstead.host}:${toString routes.haproxyBackends.thingstead.port}";
              }
            ];
          };
        }
      ];
      chains = [
        {
          name = "chain-0";
          hops = [
            {
              name = "hop-0";
              nodes = [
                {
                  name = "node-0";
                  addr = "127.0.0.1:${toString clientPort}";
                  connector = {
                    type = "tunnel";
                    metadata = {
                      "tunnel.id" = jamesLocal;
                      "tunnel.weight" = 1;
                    };
                    auth = {
                      username = "james";
                      password = "$GOST_PASSWORD";
                    };
                  };
                  dialer = {
                    type = "tcp";
                  };
                }
              ];
            }
          ];
        }
        {
          name = "chain-thingstead";
          hops = [
            {
              name = "hop-thingstead";
              nodes = [
                {
                  name = "node-thingstead";
                  addr = "127.0.0.1:${toString clientPort}";
                  connector = {
                    type = "tunnel";
                    metadata = {
                      "tunnel.id" = thingsteadLocal;
                      "tunnel.weight" = 1;
                    };
                    auth = {
                      username = "james";
                      password = "$GOST_PASSWORD";
                    };
                  };
                  dialer = {
                    type = "tcp";
                  };
                }
              ];
            }
          ];
        }
      ];

    }
  );

  sharedConfig = {
    DynamicUser = true;
    LockPersonality = true;
    NoNewPrivileges = true;
    PrivateDevices = true;
    PrivateMounts = true;
    PrivateTmp = true;
    ProtectSystem = "strict";
    ProtectHome = true;
    ProtectControlGroups = true;
    RestrictAddressFamilies = "AF_UNIX AF_INET AF_INET6";
    ProtectClock = true;
    ProtectProc = "invisible";
    ProtectHostname = true;
    ProtectKernelLogs = true;
    ProtectKernelModules = true;
    ProtectKernelTunables = true;
    RemoveIPC = true;
    RestrictNamespaces = true;
    RestrictRealtime = true;
    RestrictSUIDSGID = true;
    SystemCallFilter = [
      "@system-service"
      "~@privileged"
      "@resources"
    ];
    SystemCallArchitectures = "native";
    MemoryDenyWriteExecute = true;
  };
in
{
  options.hosts.james.ingress = {
    implementation = lib.mkOption {
      type = lib.types.enum [
        "gost"
        "haproxy"
      ];
      default = "gost";
      description = "Public ingress implementation used on james.";
    };
  };

  config = lib.mkIf (cfg.implementation == "gost") {
    networking.firewall.allowedTCPPorts = [
      443 # https
      clientPort
    ];
    networking.firewall.allowedUDPPorts = [
      443 # http3
    ];
    # File containing credentials for all the gost clients
    sops.secrets.gost-ingress-auth = { };

    # File containing the password for the james local gost client
    sops.secrets.gost-ingress-password = { };

    # This is our cloudflared-like ingress. It does TCP forwarding based on the SNI hostname
    # It will forward traffic to deweyLocal (running at home) or jamesLocal (local to james services)
    systemd.services.gost-ingress = {
      preStart = ''
        export GOST_AUTH_FILE="$CREDENTIALS_DIRECTORY/GOST_AUTH_FILE"
        ${pkgs.envsubst}/bin/envsubst \
          -o $STATE_DIRECTORY/config.json \
          -i ${gostConfig}
      '';
      script = ''
        ${lib.getExe pkgs.gost} -C $STATE_DIRECTORY/config.json
      '';
      serviceConfig = {
        AmbientCapabilities = [ "CAP_NET_BIND_SERVICE" ];
        LoadCredential = [ "GOST_AUTH_FILE:${config.sops.secrets.gost-ingress-auth.path}" ];
        StateDirectory = "gost-ingress";
      }
      // sharedConfig;
      wantedBy = [ "multi-user.target" ];
    };

    # This is a local client on james that forward to our local nginx server for handling servies that run directly on james
    # It corresponds to the jamesLocal
    systemd.services.gost-ingress-client = {
      preStart = ''
        rm -f $STATE_DIRECTORY/config.json
        export GOST_PASSWORD="$(<"$CREDENTIALS_DIRECTORY/GOST_PASSWORD")"
        ${pkgs.envsubst}/bin/envsubst \
          -o $STATE_DIRECTORY/config.json \
          -i ${gostClientConfig}
      '';
      script = ''
        ${lib.getExe pkgs.gost} -C $STATE_DIRECTORY/config.json
      '';
      serviceConfig = {
        LoadCredential = [ "GOST_PASSWORD:${config.sops.secrets.gost-ingress-password.path}" ];
        StateDirectory = "gost-ingress-client";
      }
      // sharedConfig;
      wantedBy = [ "multi-user.target" ];
    };
  };
}
