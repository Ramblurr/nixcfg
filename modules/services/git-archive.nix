{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.services.git-archive;
  onepassword = config.modules.services.onepassword-systemd-credentials;
  stateDirActual = "/var/lib/private/git-archive";
  stateDirEffective = "/var/lib/git-archive";
  gickupConfigTemplate = pkgs.writeText "gickup-config-template.yaml" ''
    ---
    source:
      github:
        - token: $GITHUB_TOKEN_RAMBLURR
          user: ramblurr
          ssh: false
          filter:
            lastactivity: 20y
            excludeforks: true
        - token: $GITHUB_TOKEN_OL
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
    assertions = [
      {
        assertion = onepassword.enable;
        message = "Git archive credentials require the 1Password systemd credential provider.";
      }
    ];

    modules.services.onepassword-systemd-credentials.consumers.gickup = {
      GITHUB_TOKEN_RAMBLURR = "op://home-ops-prod/gickup/github-token-ramblurr";
      GITHUB_TOKEN_OL = "op://home-ops-prod/gickup/github-token-ol";
    };

    systemd.timers.gickup = {
      enable = true;
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = "03:30";
        Persistent = true;
      };
    };

    site.gatus.heartbeats.git-archive = {
      service = "gickup";
      name = "Git Archive";
      group = config.site.gatus.groups.work;
      interval = "30h";
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
        export GITHUB_TOKEN_RAMBLURR="$(cat "$CREDENTIALS_DIRECTORY/GITHUB_TOKEN_RAMBLURR")"
        export GITHUB_TOKEN_OL="$(cat "$CREDENTIALS_DIRECTORY/GITHUB_TOKEN_OL")"
        ${pkgs.envsubst}/bin/envsubst \
          -o $STATE_DIRECTORY/config.yaml \
          -i ${gickupConfigTemplate}
      '';
      script = ''
        ${lib.getExe pkgs.gickup} $STATE_DIRECTORY/config.yaml
      '';
      serviceConfig = {
        Type = "oneshot";
        DynamicUser = true;
        StateDirectory = baseNameOf stateDirEffective;
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
