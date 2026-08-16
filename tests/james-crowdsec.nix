{
  inputs,
  pkgs,
}:
let

  evaluated = inputs.nixpkgs.lib.nixosSystem {
    modules = [
      ../hosts/james/crowdsec.nix
      (
        { lib, ... }:
        {
          options = {
            repo.secrets = lib.mkOption { type = lib.types.attrs; };
            site.net = lib.mkOption {
              type = lib.types.attrs;
              default = { };
            };
            sops.secrets = lib.mkOption {
              type = lib.types.attrsOf (
                lib.types.submodule (
                  { name, ... }:
                  {
                    options = {
                      path = lib.mkOption {
                        type = lib.types.str;
                        default = "/run/secrets/${name}";
                      };
                      owner = lib.mkOption {
                        type = lib.types.str;
                        default = "root";
                      };
                      group = lib.mkOption {
                        type = lib.types.str;
                        default = "root";
                      };
                      mode = lib.mkOption {
                        type = lib.types.str;
                        default = "0400";
                      };
                      restartUnits = lib.mkOption {
                        type = lib.types.listOf lib.types.str;
                        default = [ ];
                      };
                    };
                  }
                )
              );
              default = { };
            };
            environment.persistence = lib.mkOption {
              type = lib.types.attrs;
              default = { };
            };
          };

          config = {
            nixpkgs.pkgs = pkgs;
            networking.hostName = "james";
            system.stateVersion = "25.11";
            services.openssh.enable = true;
            services.caddy.enable = true;
            services.tailscale.enable = true;
            repo.secrets = {
              global.domain.tailnet = "example.test";
              local.crowdsec.trustedSourceCidrs = [ "100.64.0.0/10" ];
            };
          };
        }
      )
    ];
  };

  cfg = evaluated.config;
  crowdsec = cfg.systemd.services.crowdsec;
  bouncer = cfg.systemd.services.crowdsec-firewall-bouncer;
  parserCleanup =
    cfg.systemd.tmpfiles.settings."09-crowdsec-local-parser-cleanup"."/etc/crowdsec/parsers/s02-enrich/*-parsers-s02-enrich.yaml".r;
  expectedAcquisitions = [
    {
      source = "journalctl";
      journalctl_filter = [ "_SYSTEMD_UNIT=sshd.service" ];
      labels.type = "syslog";
    }
    {
      source = "journalctl";
      journalctl_filter = [ "_TRANSPORT=kernel" ];
      labels.type = "kernel";
    }
    {
      source = "file";
      filenames = [ "/var/log/caddy/access.log" ];
      labels.type = "caddy";
    }
  ];
in
assert cfg.services.crowdsec.settings.general.api.server.enable == false;
assert
  cfg.services.crowdsec.settings.lapi.credentialsFile == "/run/secrets/crowdsec/lapiCredentials";
assert cfg.services.crowdsec.localConfig.acquisitions == expectedAcquisitions;
assert
  cfg.services.crowdsec.hub.collections == [
    "crowdsecurity/linux"
    "crowdsecurity/sshd"
    "crowdsecurity/caddy"
    "crowdsecurity/http-dos"
  ];
assert builtins.elem "caddy" cfg.users.users.crowdsec.extraGroups;
assert cfg.services.crowdsec-firewall-bouncer.settings.api_url == "http://addams.example.test:6001";
assert cfg.services.crowdsec-firewall-bouncer.settings.mode == "iptables";
assert cfg.services.crowdsec-firewall-bouncer.registerBouncer.enable == false;
assert
  cfg.services.crowdsec-firewall-bouncer.secrets.apiKeyPath == "/run/secrets/crowdsec/bouncerApiKey";
assert bouncer.serviceConfig.LoadCredential == "API_KEY_FILE:/run/secrets/crowdsec/bouncerApiKey";
assert
  cfg.systemd.services.crowdsec-update-hub.serviceConfig.ExecStartPost == [
    "+${pkgs.systemd}/bin/systemctl --no-block try-reload-or-restart crowdsec.service"
  ];
assert crowdsec.serviceConfig.Restart == "on-failure";
assert crowdsec.serviceConfig.RestartSec == "5s";
assert crowdsec.serviceConfig.RestartSteps == 5;
assert crowdsec.serviceConfig.RestartMaxDelaySec == "60s";
assert bouncer.serviceConfig.Restart == "on-failure";
assert bouncer.serviceConfig.RestartSec == "5s";
assert bouncer.serviceConfig.RestartSteps == 5;
assert bouncer.serviceConfig.RestartMaxDelaySec == "60s";
assert builtins.elem "tailscaled.service" crowdsec.after;
assert builtins.elem "tailscaled.service" crowdsec.wants;
assert builtins.elem "network-online.target" bouncer.after;
assert builtins.elem "tailscaled.service" bouncer.after;
assert builtins.elem "network-online.target" bouncer.wants;
assert builtins.elem "tailscaled.service" bouncer.wants;
assert !(builtins.elem "crowdsec.service" bouncer.after);
assert !(builtins.elem "crowdsec.service" bouncer.wants);
assert !(builtins.elem "crowdsec.service" bouncer.requires);
assert !(builtins.elem "firewall.service" bouncer.after);
assert !(builtins.elem "firewall.service" bouncer.wants);
assert !(builtins.elem "firewall.service" bouncer.partOf);
assert parserCleanup.type == "r";
pkgs.runCommand "james-crowdsec-module-test" { nativeBuildInputs = [ pkgs.jq ]; } ''
  cat > "$TMPDIR/caddy-access.json" <<'JSON'
  {"level":"info","ts":1786816800.0,"logger":"http.log.access","msg":"handled request","request":{"remote_ip":"198.51.100.25","remote_port":"4242","client_ip":"198.51.100.25","proto":"HTTP/2.0","method":"GET","host":"work.example.test","uri":"/login","headers":{"User-Agent":["representative-agent"]}},"bytes_read":0,"duration":0.001,"size":123,"status":404}
  JSON
  jq -e '
    .logger == "http.log.access" and
    .request.client_ip == "198.51.100.25" and
    .request.proto == "HTTP/2.0" and
    .request.method == "GET" and
    .request.host == "work.example.test" and
    .request.uri == "/login" and
    .request.headers["User-Agent"][0] == "representative-agent" and
    .status == 404
  ' "$TMPDIR/caddy-access.json"
  touch "$out"
''
