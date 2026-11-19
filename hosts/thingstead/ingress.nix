{
  config,
  lib,
  ...
}:

let
  hirdeT3Port = config.home-manager.users.hirde.my.services.t3code.port;
  matrixSettings = config.services.matrix-synapse.settings;
  matrixServerName = matrixSettings.server_name;
  matrixPublicHost = lib.removePrefix "https://" (lib.removeSuffix "/" matrixSettings.public_baseurl);
  matrixPort = toString (builtins.head matrixSettings.listeners).port;
in
{
  services.caddy = {
    enable = true;
    email = "security@moot.land";
    virtualHosts.${matrixServerName}.extraConfig =
      let
        server = {
          "m.server" = "${matrixPublicHost}:443";
        };
      in
      ''
        handle /.well-known/matrix/server {
          header Content-Type application/json
          respond ${builtins.toJSON server} 200
        }

        handle /.well-known/matrix/client {
          reverse_proxy 127.0.0.1:${matrixPort}
        }

        handle {
          header Content-Type text/plain
          respond "Hello world" 200
        }
      '';

    virtualHosts.${matrixPublicHost}.extraConfig = ''
      reverse_proxy 127.0.0.1:${matrixPort}
    '';
  };

  # Foreground Serve owns only this port and withdraws it on service stop.
  systemd.services.t3code-hirde-tailscale-serve = {
    description = "Tailscale HTTPS listener for hirde's T3 Code";
    wantedBy = [ "multi-user.target" ];
    after = [ "tailscaled.service" ];
    requires = [ "tailscaled.service" ];
    serviceConfig = {
      ExecStart = "${lib.getExe config.services.tailscale.package} serve --https=8443 http://127.0.0.1:${toString hirdeT3Port}";
      Restart = "on-failure";
      RestartSec = 5;
    };
  };

  modules.zfs.datasets.properties = {
    "rpool/encrypted/safe/svc/caddy"."mountpoint" = "/var/lib/caddy";
  };

  networking.firewall.allowedTCPPorts = [
    80
    443
  ];
}
