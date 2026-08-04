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
            services.nginx.enable = true;
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
      filenames = [ "/var/log/nginx/crowdsec.log" ];
      labels.type = "nginx";
    }
  ];
in
assert cfg.services.crowdsec.settings.general.api.server.enable == false;
assert
  cfg.services.crowdsec.settings.lapi.credentialsFile == "/run/secrets/crowdsec/lapiCredentials";
assert cfg.services.crowdsec.localConfig.acquisitions == expectedAcquisitions;
assert cfg.services.crowdsec-firewall-bouncer.settings.api_url == "http://addams.example.test:6001";
assert cfg.services.crowdsec-firewall-bouncer.settings.mode == "iptables";
assert cfg.services.crowdsec-firewall-bouncer.registerBouncer.enable == false;
assert
  cfg.services.crowdsec-firewall-bouncer.secrets.apiKeyPath == "/run/secrets/crowdsec/bouncerApiKey";
assert bouncer.serviceConfig.LoadCredential == "API_KEY_FILE:/run/secrets/crowdsec/bouncerApiKey";
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
pkgs.runCommand "james-crowdsec-module-test" { } ''
  touch "$out"
''
