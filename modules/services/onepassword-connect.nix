{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.services.onepassword-connect;
  replicaHosts = [
    "debord"
    "dewey"
    "mali"
  ];
  tls = import ../../configs/home-ops/onepassword-connect-tls.nix;
  replicaAddresses = map (name: builtins.head config.site.net.mgmt.hosts4.${name}) replicaHosts;
  localAddress = builtins.head config.site.net.mgmt.hosts4.${config.networking.hostName};
  dataDir = "/var/lib/onepassword-connect";
  credentials = config.sops.secrets."onepassword-connect/credentials";
  tlsKey = config.sops.secrets."onepassword-connect/tls-key";
  heartbeat = "${pkgs.curl}/bin/curl --fail --silent --show-error --max-time 2 --cacert ${tls.certificate} ${tls.url "127.0.0.1"}/heartbeat";
  peerSet = lib.concatStringsSep ", " cfg.availability.peerAddresses;
in
{
  options.modules.services.onepassword-connect = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = builtins.elem config.networking.hostName replicaHosts;
      readOnly = true;
      description = "Whether this host is one of the designated Connect replicas.";
    };
    credentialsFile = lib.mkOption {
      type = lib.types.path;
      default = ../../configs/home-ops/shared.sops.yml;
      description = "SOPS file containing the shared Connect workflow credentials.";
    };
    cacheDataset = lib.mkOption {
      type = lib.types.str;
      default =
        if config.networking.hostName == "mali" then
          "rpool2/encrypted/safe/svc/onepassword-connect"
        else
          "rpool/encrypted/safe/svc/onepassword-connect";
      description = "Host-local ZFS dataset used for the persistent Connect cache.";
    };
    availability = {
      interface = lib.mkOption {
        type = lib.types.str;
        default = "mgmt";
        description = "Management interface that carries the availability endpoint.";
      };
      localAddress = lib.mkOption {
        type = lib.types.str;
        default = localAddress;
        description = "Replica address used as the unicast VRRP source.";
      };
      peerAddresses = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = builtins.filter (address: address != localAddress) replicaAddresses;
        description = "Management addresses of the other Connect replicas.";
      };
      virtualAddress = lib.mkOption {
        type = lib.types.str;
        default = builtins.head config.site.net.mgmt.hosts4.onepassword-connect;
        description = "Management-network address held by the active replica.";
      };
    };
    tailnetUrl = lib.mkOption {
      type = lib.types.str;
      readOnly = true;
      default = tls.tailnetUrl config.repo.secrets.global.domain.tailnet;
      defaultText = lib.literalMD "the op-connect Tailscale Service";
      description = "Connect URL for clients outside the management network, served with the tailnet's certificate.";
    };
    ports.sync = lib.mkOption {
      type = lib.types.port;
      default = config.repo.secrets.home-ops.ports.onepassword-connect-sync;
    };
    user = lib.mkOption {
      type = lib.types.unspecified;
      default = config.repo.secrets.home-ops.users.onepassword-connect;
    };
    group = lib.mkOption {
      type = lib.types.unspecified;
      default = config.repo.secrets.home-ops.groups.onepassword-connect;
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = !(builtins.elem cfg.availability.localAddress cfg.availability.peerAddresses);
        message = "The local Connect replica address must not appear in its VRRP peer list.";
      }
      {
        assertion = cfg.availability.localAddress != cfg.availability.virtualAddress;
        message = "The Connect availability endpoint must not be a permanent replica address.";
      }
    ];

    users.users.${cfg.user.name} = {
      inherit (cfg.user) name;
      uid = lib.mkForce cfg.user.uid;
      isNormalUser = true;
      group = lib.mkForce cfg.group.name;
      home = dataDir;
      shell = pkgs.bashInteractive;
      linger = true;
      createHome = false;
      autoSubUidGidRange = true;
    };

    users.groups.${cfg.group.name} = {
      inherit (cfg.group) name;
      gid = lib.mkForce cfg.group.gid;
    };

    home-manager.users.${cfg.user.name} =
      { config, ... }:
      {
        home.homeDirectory = dataDir;
        home.sessionVariables = {
          DBUS_SESSION_BUS_ADDRESS = "unix:path=/run/user/${toString cfg.user.uid}/bus";
          XDG_RUNTIME_DIR = "/run/user/${toString cfg.user.uid}";
        };
        programs.bash = {
          enable = true;
          initExtra = ''
            [[ -f "${config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh" ]] && source "${config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh"
          '';
        };
      };

    sops.secrets."onepassword-connect/credentials" = {
      sopsFile = cfg.credentialsFile;
      owner = cfg.user.name;
      group = cfg.group.name;
      mode = "0400";
    };

    sops.secrets."onepassword-connect/tls-key" = {
      sopsFile = tls.keyFile;
      owner = cfg.user.name;
      group = cfg.group.name;
      mode = "0400";
    };

    systemd.tmpfiles.rules = [ "d ${dataDir} 0770 ${cfg.user.name} ${cfg.group.name}" ];

    modules.zfs.datasets.properties = {
      "${cfg.cacheDataset}" = {
        mountpoint = dataDir;
        "com.sun:auto-snapshot" = "false";
      };
    };

    networking.firewall = {
      interfaces.${cfg.availability.interface}.allowedTCPPorts = [ tls.port ];
      extraInputRules = ''
        iifname "${cfg.availability.interface}" ip saddr { ${peerSet} } ip daddr ${cfg.availability.localAddress} meta l4proto vrrp accept comment "Connect replica VRRP peers"
      '';
    };

    services.tailscale.serve = {
      enable = true;
      services.op-connect.endpoints."tcp:443" = tls.url cfg.availability.virtualAddress;
    };

    systemd.services.tailscaled.environment.SSL_CERT_DIR = "${tls.trustDirectory pkgs tls.certificate}";

    services.keepalived = {
      enable = true;
      enableScriptSecurity = true;
      vrrpScripts.onepassword-connect-heartbeat = {
        script = "${heartbeat}";
        user = cfg.user.name;
        group = cfg.group.name;
        interval = 5;
        timeout = 3;
        fall = 1;
        rise = 1;
      };
      vrrpInstances.onepassword-connect = {
        interface = cfg.availability.interface;
        state = "BACKUP";
        virtualRouterId = 42;
        priority = 100;
        noPreempt = true;
        unicastSrcIp = cfg.availability.localAddress;
        unicastPeers = cfg.availability.peerAddresses;
        virtualIps = [ { addr = cfg.availability.virtualAddress; } ];
        trackScripts = [ "onepassword-connect-heartbeat" ];
      };
    };

    virtualisation.quadlet = {
      enable = true;
      pods.op-connect = {
        inherit (cfg.user) uid;
        autoStart = true;
        podConfig = {
          PodName = "op-connect";
          PublishPort = [
            "${toString tls.port}:${toString tls.port}"
            "127.0.0.1:${toString cfg.ports.sync}:8081"
          ];
          DNS = [ (builtins.head config.repo.secrets.global.nameservers) ];
          UserNS = "keep-id:uid=999,gid=999";
        };
      };
      containers = {
        op-connect-api = {
          inherit (cfg.user) uid;
          autoStart = true;
          serviceConfig = {
            ExecStartPre = [
              "${pkgs.coreutils}/bin/test -r ${credentials.path}"
              "${pkgs.coreutils}/bin/test -r ${tlsKey.path}"
            ];
            RestartSec = "10";
            Restart = "always";
          };
          containerConfig = {
            # renovate: docker-image
            Image = "docker.io/1password/connect-api:1.8.2@sha256:e915c0c843972f02b0e7e2de502bda8bd4a092288b3f1866098a857bd715a281";
            Environment = [
              "XDG_DATA_HOME=/config"
              "OP_BUS_PORT=11220"
              "OP_BUS_PEERS=localhost:11221"
              "OP_SESSION=/config/1password-credentials.json"
              "OP_HTTPS_PORT=${toString tls.port}"
              "OP_TLS_CERT_FILE=/tls/certificate.pem"
              "OP_TLS_KEY_FILE=/tls/key.pem"
            ];
            Pod = "op-connect.pod";
            Volume = [
              "${dataDir}:/config:rw"
              "${credentials.path}:/config/1password-credentials.json:ro"
              "${tls.certificate}:/tls/certificate.pem:ro"
              "${tlsKey.path}:/tls/key.pem:ro"
            ];
          };
        };
        op-connect-sync = {
          inherit (cfg.user) uid;
          autoStart = true;
          serviceConfig = {
            ExecStartPre = [ "${pkgs.coreutils}/bin/test -r ${credentials.path}" ];
            RestartSec = "2";
            Restart = "always";
          };
          containerConfig = {
            # renovate: docker-image
            Image = "docker.io/1password/connect-sync:1.8.2@sha256:6297ca6136c0f0fb096bc64c49e1bc8df2aab35282ebff8c7bb60745ef176d0d";
            Environment = [
              "XDG_DATA_HOME=/config"
              "OP_BUS_PORT=11221"
              "OP_HTTP_PORT=8081"
              "OP_BUS_PEERS=localhost:11220"
              "OP_SESSION=/config/1password-credentials.json"
            ];
            Pod = "op-connect.pod";
            Volume = [
              "${dataDir}:/config:rw"
              "${credentials.path}:/config/1password-credentials.json:ro"
            ];
          };
        };
      };
    };
  };
}
