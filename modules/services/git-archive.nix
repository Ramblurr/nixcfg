{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.services.git-archive;
  stateDirActual = "/var/lib/private/git-archive";
  stateDirEffective = "/var/lib/git-archive";
in
{
  options.modules.services.git-archive = {
    enable = lib.mkEnableOption "git-archive";
  };

  config = lib.mkIf cfg.enable {
    modules.zfs.datasets.properties = {
      "tank/svc/git-archive"."mountpoint" = stateDirActual;
    };
    environment.systemPackages = [ pkgs.gickup ];
    sops.secrets = {
      "gickup/github-token-ramblurr".sopsFile = ../../configs/home-ops/shared.sops.yml;
      "gickup/github-token-ol".sopsFile = ../../configs/home-ops/shared.sops.yml;
    };
    sops.templates."gickup-config.yaml" = {
      mode = "0400";
      content = ''
        ---
        source:
          github:
            - token: ${config.sops.placeholder."gickup/github-token-ramblurr"}
              user: ramblurr
              ssh: false
              filter:
                lastactivity: 20y
                excludeforks: true
            - token: ${config.sops.placeholder."gickup/github-token-ol"}
              user: outskirtslabs
              ssh: false
              filter:
                lastactivity: 20y
                excludeforks: true
        destination:
          local:
            - path: $STATE_DIRECTORY/archive
              structured: true
              zip: true
              keep: 5
              bare: true
              lfs: false
        log:
          file-logging:
            dir: $STATE_DIRECTORY/logs
            file: gickup.log
            maxage: 7
      '';
    };

    systemd.timers.gickup = {
      enable = true;
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = "03:30";
        Persistent = true;
      };
    };
    systemd.services.gickup = {
      enable = true;
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      unitConfig = {
        RequiresMountsFor = [ stateDirActual ];
      };
      preStart = ''
        mkdir -p $STATE_DIRECTORY/archive
        mkdir -p $STATE_DIRECTORY/logs
        ${pkgs.envsubst}/bin/envsubst \
          -o $STATE_DIRECTORY/config.yaml \
          -i $CREDENTIALS_DIRECTORY/config.yaml
      '';
      script = ''
        ${lib.getExe pkgs.gickup} $STATE_DIRECTORY/config.yaml
      '';
      serviceConfig = {
        Type = "oneshot";
        DynamicUser = true;
        StateDirectory = baseNameOf stateDirEffective;
        LoadCredential = [ "config.yaml:${config.sops.templates."gickup-config.yaml".path}" ];
        UMask = 77;
        DeviceAllow = "";
        LockPersonality = true;
        NoNewPrivileges = true;
        PrivateDevices = true;
        PrivateTmp = true;
        PrivateUsers = true;
        ProcSubset = "pid";
        ProtectControlGroups = true;
        ProtectClock = true;
        ProtectHome = true;
        ProtectHostname = true;
        ProtectKernelLogs = true;
        ProtectKernelModules = true;
        ProtectKernelTunables = true;
        ProtectProc = "invisible";
        ProtectSystem = "strict";
        RemoveIPC = true;
        RestrictNamespaces = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        SystemCallArchitectures = "native";
        SystemCallFilter = [ "@system-service" ];
      };
    };
  };
}
