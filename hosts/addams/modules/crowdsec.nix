{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  inherit (config.networking) hostName;
  configPath = "/var/lib/crowdsec/config";
  crowdsecPort = toString 6001;
  mkAcquisition =
    enable: unit:
    if enable then
      {
        source = "journalctl";
        journalctl_filter = [ "_SYSTEMD_UNIT=${unit}" ];
        labels.type = "syslog";
      }
    else
      null;

in
{
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
  };
  services.crowdsec = {
    enable = true;
    enrollKeyFile = config.sops.secrets."crowdsec/enrollKey".path;
    allowLocalJournalAccess = true;
    acquisitions = builtins.filter (v: v != null) [
      (mkAcquisition config.services.openssh.enable "sshd.service")
      {
        source = "journalctl";
        journalctl_filter = [ "_TRANSPORT=kernel" ];
        labels.type = "kernel";
      }
    ];
    settings = {
      api.server = {
        listen_uri = "127.0.0.1:${crowdsecPort}";
      };
      cscli = {
        output = "human";
      };
      config_paths = {
        #simulation_path = "${configPath}/simulation.yaml";
      };
    };
  };
  systemd.services.crowdsec.serviceConfig = {
    ExecStartPre = lib.mkAfter [
      (pkgs.writeShellScript "crowdsec-packages" ''
        cscli hub upgrade

        cscli collections install \
          crowdsecurity/linux \
          crowdsecurity/sshd \
          crowdsecurity/base-http-scenarios

        cscli parsers install \
          crowdsecurity/iptables-logs

        #echo "simulation: false" > ${configPath}/simulation.yaml
        #cscli simulation enable crowdsecurity/http-bad-user-agent
        #cscli simulation enable crowdsecurity/http-crawl-non_statics
        #cscli simulation enable crowdsecurity/http-probing
      '')
    ];
    ExecStartPost =
      let
        script = pkgs.writeScriptBin "register-bouncer" ''
          #!${pkgs.runtimeShell}
          set -eu
          set -o pipefail

          while ! cscli lapi status; do
            echo "Waiting for CrowdSec daemon to be ready"
            sleep 5
          done

          if ! cscli bouncers list | grep -q "${hostName}-bouncer"; then
            cscli bouncers add "${hostName}-bouncer" --key "cs-firewall-bouncer-key"
          fi
        '';
      in
      [ "${script}/bin/register-bouncer" ];
  };
  services.crowdsec-firewall-bouncer = {
    enable = true;
    settings = {
      api_key = "cs-firewall-bouncer-key";
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
