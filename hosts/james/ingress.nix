{
  config,
  lib,
  pkgs,
  ...
}:

# Reverse Proxy and Tunnel Service (alternative to Cloudflare Tunnel)
#
# This module configures a secure tunnel service using gost that:
# 1. Listens for incoming HTTPS traffic on the VPS (james)
# 2. Routes traffic based on domain names either to:
#    - Local services on the VPS via nginx
#    - Home network services via a secure tunnel to dewey (home server)
# 3. Allows hosting services on the home network without exposing ports
#    through the firewall, similar to Cloudflare Tunnel
#
# The home server (dewey) maintains a persistent connection to this VPS,
# enabling two-way communication through the firewall.
let
  inherit (config.repo.secrets.global.ingressTunnel)
    jamesLocal
    deweyLocal
    clientPort
    ;
  inherit (config.repo.secrets.global.domain)
    home
    work
    work2
    personal1
    personal2
    personal3
    personal4
    personal5
    personal6
    family
    ;
  deweyServices = [
    "dav.${home}"
    "home.${home}"
    "books.${home}"
    "auth.${home}"
    "clients.${work}"
    "auth.${work}"
    "matrix.${work}"
    "data.${work}"
  ];
  localServices = [
    "${work}"
    ".${work}"
    "${work2}"
    ".${work2}"
    "${personal1}"
    ".${personal1}"
    "${personal2}"
    ".${personal2}"
    "${personal3}"
    ".${personal3}"
    "${personal4}"
    ".${personal4}"
    "${personal5}"
    ".${personal5}"
    "${personal6}"
    ".${personal6}"
    "${family}"
    ".${family}"
  ];

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
              hostname = hostname;
              endpoint = deweyLocal;
            }) deweyServices)
            ++ (map (hostname: {
              hostname = hostname;
              endpoint = jamesLocal;
            }) localServices);
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
    } // sharedConfig;
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
    } // sharedConfig;
    wantedBy = [ "multi-user.target" ];
  };

}
