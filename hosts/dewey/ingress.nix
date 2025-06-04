{
  config,
  lib,
  pkgs,
  ...
}:
# an alternative to cloudflare tunnel
# The server (james) listens on ports 80 and 443
# and forwards traffic to the client (dewey) that connects over gost
# see hosts/james/ingress.nix for the counterpart config
let
  inherit (config.repo.secrets.global) jamesIp;
  inherit (config.repo.secrets.global)
    deweyLocalTunnel
    ;
  gostConfig = pkgs.writeText "gost.json" (
    builtins.toJSON {
      log = {
        #level = "debug";
        format = "text";
      };
      services = [
        {
          name = "james-ingress";
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
                addr = "127.0.0.1:443";
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
                  addr = "${jamesIp}:3434";
                  connector = {
                    type = "tunnel";
                    metadata = {
                      "tunnel.id" = deweyLocalTunnel;
                      "tunnel.weight" = 1;
                    };
                    auth = {
                      username = "dewey";
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

in
{
  sops.secrets.gost-ingress-password = { };
  systemd.services.gost-ingress-client = {
    preStart = ''
      rm -f $STATE_DIRECTORY/config.json
      export GOST_PASSWORD="$(<"$CREDENTIALS_DIRECTORY/GOST_PASSWORD")"
      echo "Generating gost ingress config"
      echo "GOST_PASSWORD: $GOST_PASSWORD"
      ${pkgs.envsubst}/bin/envsubst \
        -o $STATE_DIRECTORY/config.json \
        -i ${gostConfig}
    '';
    script = ''
      ${lib.getExe pkgs.gost} -C $STATE_DIRECTORY/config.json
    '';
    serviceConfig = {
      LoadCredential = [ "GOST_PASSWORD:${config.sops.secrets.gost-ingress-password.path}" ];
      DynamicUser = true;
      StateDirectory = "gost-ingress-client";
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
    wantedBy = [ "multi-user.target" ];
  };

}
