{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.rsyncnet-zrepl-reconcile;
  serviceName = "rsyncnet-zrepl-metrics-tunnel";
  listenAddress = builtins.head config.site.net.mgmt.hosts4.mali;
  prometheusAddress = builtins.head config.site.net.mgmt.hosts4.debord;
in
{
  options.services.rsyncnet-zrepl-reconcile.metricsTunnel.enable =
    lib.mkEnableOption "the SSH tunnel for rsync.net zrepl metrics";

  config = lib.mkIf cfg.metricsTunnel.enable {
    assertions = [
      {
        assertion = cfg.enable;
        message = "The rsync.net metrics tunnel reuses the enabled reconciler's credentials.";
      }
      {
        assertion = config.networking.nftables.enable;
        message = "The rsync.net metrics tunnel requires the source-restricted nftables firewall rule.";
      }
    ];

    modules.services.onepassword-systemd-credentials.consumers.${serviceName} = {
      identity = cfg.identityReference;
      known-hosts = cfg.knownHostsReference;
    };

    networking.firewall.extraInputRules = ''
      iifname "mgmt" ip saddr ${prometheusAddress} ip daddr ${listenAddress} tcp dport 9812 accept
    '';

    systemd.services.${serviceName} = {
      description = "Tunnel rsync.net zrepl metrics to Mali's management address";
      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      serviceConfig = {
        DynamicUser = true;
        ExecStart = lib.escapeShellArgs [
          "${pkgs.openssh}/bin/ssh"
          "-F"
          "/dev/null"
          "-N"
          "-T"
          "-o"
          "BatchMode=yes"
          "-o"
          "IdentitiesOnly=yes"
          "-o"
          "IdentityAgent=none"
          "-o"
          "StrictHostKeyChecking=yes"
          "-o"
          "UserKnownHostsFile=%d/known-hosts"
          "-o"
          "GlobalKnownHostsFile=/dev/null"
          "-o"
          "ExitOnForwardFailure=yes"
          "-o"
          "ConnectTimeout=15"
          "-o"
          "ServerAliveInterval=30"
          "-o"
          "ServerAliveCountMax=3"
          "-i"
          "%d/identity"
          "-L"
          "${listenAddress}:9812:127.0.0.1:9811"
          "root@${cfg.receiverHost}"
        ];
        Restart = "always";
        RestartSec = "15s";
        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectHome = true;
        ProtectSystem = "strict";
        CapabilityBoundingSet = "";
        RestrictAddressFamilies = [
          "AF_UNIX"
          "AF_INET"
          "AF_INET6"
        ];
      };
    };
  };
}
