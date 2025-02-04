{
  config,
  pkgs,
  lib,
  ...
}:
{

  sops.secrets."maddy-smtp-relay" = { };
  containers.maddy = {
    autoStart = true;
    ephemeral = true;
    privateNetwork = true;
    hostAddress = "10.4.0.1";
    localAddress = "10.4.0.2";
    bindMounts."/var/run/secrets/maddy-env".hostPath = config.sops.secrets."maddy-smtp-relay".path;
    config =
      { config, pkgs, ... }:
      {
        system.stateVersion = "24.11";
        networking = {
          useHostResolvConf = lib.mkForce false;
          firewall = {
            #enable = true;
            allowedTCPPorts = [ 25 ];
          };
        };
        services.resolved.enable = true;
        services.maddy = {
          enable = true;
          hostname = "{env:SMTP_DOMAIN}";
          primaryDomain = "{env:SMTP_DOMAIN}";
          tls.loader = "off";

          secrets = [
            "/var/run/secrets/maddy-env"
          ];

          config = ''
            state_dir /run/maddy/state
            runtime_dir /run/maddy/run
            openmetrics tcp://0.0.0.0:8080 { }

            smtp tcp://0.0.0.0:25 {
                debug true
                io_debug true
                source {env:SMTP_DOMAIN} {
                    default_destination {
                      deliver_to &remote_queue
                    }
                }
                source {env:SMTP_DOMAIN_WORK} {
                    default_destination {
                      deliver_to &remote_queue_work
                    }
                }
                default_source {
                    reject 521 5.0.0 "Domain not local"
                }
            }

            target.queue remote_queue {
                debug true
                target &remote_smtp
            }

            target.smtp remote_smtp {
                debug true
                attempt_starttls yes
                require_tls yes
                auth plain {env:SMTP_USERNAME} {env:SMTP_PASSWORD}
                targets tls://{env:SMTP_SERVER}:{env:SMTP_PORT}
            }

            target.queue remote_queue_work {
                debug true
                target &remote_smtp_work
            }

            target.smtp remote_smtp_work {
                debug true
                attempt_starttls yes
                require_tls yes
                auth plain {env:SMTP_USERNAME_WORK} {env:SMTP_PASSWORD_WORK}
                targets tls://{env:SMTP_SERVER_WORK}:{env:SMTP_PORT_WORK}
            }
          '';
        };
      };
  };
}
