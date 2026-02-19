{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (config.networking) hostName;
  crowdsecPort = toString 6001;
  crowdsecDataPath = "/var/lib/crowdsec/data";
  crowdsecHubPath = "/var/lib/crowdsec/hub";
  lapiCredentialsPath = "/var/lib/crowdsec/local_api_credentials.yaml";
  capiCredentialsPath = "/var/lib/crowdsec/online_api_credentials.yaml";
  consoleConfigPath = "/var/lib/crowdsec/console.yaml";

in
{
  environment.systemPackages = [
    (pkgs.writeShellScriptBin "csli" ''
      exec /run/current-system/sw/bin/cscli "$@"
    '')
  ];

  modules.zfs.datasets.properties = {
    "rpool/svc/crowdsec"."mountpoint" = "/var/lib/crowdsec";
  };

  sops.secrets = {
    "crowdsec/enrollKey" = {
      owner = "crowdsec";
      restartUnits = [
        "crowdsec.service"
      ];
    };
    "crowdsec/apiKey" = {
      owner = "crowdsec";
      restartUnits = [
        "crowdsec.service"
        "crowdsec-firewall-bouncer.service"
      ];
    };
  };
  services.crowdsec = {
    enable = true;
    autoUpdateService = true;
    hub.collections = [
      "crowdsecurity/linux"
      "crowdsecurity/sshd"
      "crowdsecurity/base-http-scenarios"
    ];
    hub.parsers = [
      "crowdsecurity/iptables-logs"
    ];
    localConfig.acquisitions =
      (lib.optionals config.services.openssh.enable [
        {
          source = "journalctl";
          journalctl_filter = [ "_SYSTEMD_UNIT=sshd.service" ];
          labels.type = "syslog";
        }
      ])
      ++ [
        {
          source = "journalctl";
          journalctl_filter = [ "_TRANSPORT=kernel" ];
          labels.type = "kernel";
        }
      ];
    settings.general = {
      api.server = {
        enable = true;
        # Expose LAPI beyond localhost so remote tailnet nodes can authenticate.
        # Firewall policy still controls who can actually reach this port.
        listen_uri = "0.0.0.0:${crowdsecPort}";
        console_path = consoleConfigPath;
      };
      cscli.output = "human";
      config_paths = {
        data_dir = lib.mkForce crowdsecDataPath;
        hub_dir = lib.mkForce crowdsecHubPath;
        index_path = lib.mkForce "${crowdsecHubPath}/.index.json";
      };
      db_config = {
        db_path = lib.mkForce "${crowdsecDataPath}/crowdsec.db";
      };
    };
    # Required while api.server.enable = true; otherwise upstream setup script
    # attempts to stringify a null credentials path.
    settings.lapi.credentialsFile = lapiCredentialsPath;
    settings.capi.credentialsFile = capiCredentialsPath;
  };

  systemd.services.crowdsec.serviceConfig = {
    ExecStartPre = lib.mkAfter [
      (pkgs.writeShellScript "crowdsec-console-enroll" ''
        set -eu
        set -o pipefail

        cscli=/run/current-system/sw/bin/cscli
        grep=${lib.getExe pkgs.gnugrep}
        cat=${lib.getExe' pkgs.coreutils "cat"}

        if ! "$grep" -q password "${capiCredentialsPath}" 2>/dev/null; then
          "$cscli" capi register
        fi

        if [ ! -e "${consoleConfigPath}" ]; then
          "$cscli" console enroll "$("$cat" ${
            config.sops.secrets."crowdsec/enrollKey".path
          })" --name ${hostName}
        fi
      '')
    ];
    ExecStartPost = lib.mkAfter [
      (pkgs.writeShellScript "crowdsec-register-bouncer" ''
        set -eu
        set -o pipefail

        cscli=/run/current-system/sw/bin/cscli
        grep=${lib.getExe pkgs.gnugrep}
        cat=${lib.getExe' pkgs.coreutils "cat"}
        bouncerName="${hostName}-bouncer"
        bouncerKey="$("$cat" ${config.sops.secrets."crowdsec/apiKey".path})"

        while ! "$cscli" lapi status >/dev/null 2>&1; do
          sleep 1
        done

        if "$cscli" bouncers list | "$grep" -q "$bouncerName"; then
          "$cscli" bouncers delete "$bouncerName" || true
        fi

        "$cscli" bouncers add "$bouncerName" --key "$bouncerKey"
      '')
    ];
  };
  services.crowdsec-firewall-bouncer = {
    enable = true;
    createRulesets = false;
    registerBouncer.enable = false;
    secrets.apiKeyPath = config.sops.secrets."crowdsec/apiKey".path;
    settings = {
      api_url = "http://localhost:${crowdsecPort}";
      mode = "nftables";
      log_mode = "stdout";
      update_frequency = "10s";
      nftables = {
        ipv4 = {
          set-only = true;
          table = "firewall";
        };
        ipv6 = {
          set-only = true;
          table = "firewall";
        };
      };
    };
  };
  networking.nftables.firewall.zones.crowdsec-ban = {
    ingressExpression = [
      "ip saddr @crowdsec-blacklists"
      "ip6 saddr @crowdsec6-blacklists"
    ];
    egressExpression = [
      "ip daddr @crowdsec-blacklists"
      "ip6 daddr @crowdsec6-blacklists"
    ];
  };
  networking.nftables.firewall.rules.crowdsec-ban = {
    from = [ "crowdsec-ban" ];
    to = "all";
    ruleType = "ban";
    extraLines = [
      "counter drop"
    ];
  };
  networking.nftables.chains.conntrack.cs-block.rules = [
    "ip saddr @crowdsec-blacklists reject"
    "ip6 saddr @crowdsec6-blacklists reject"
  ];

}
