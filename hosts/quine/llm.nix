{
  pkgs,
  config,
  ...
}:
let
  tailscaleIp4 = "100.93.18.79";
  primAddress = "10.9.4.3";
in
{
  # pi-web tailscale proxy
  systemd.sockets.pi-web-tailscale-proxy = {
    description = "PI WEB proxy socket on external addresses";
    wantedBy = [ "sockets.target" ];
    listenStreams = [
      "${tailscaleIp4}:${toString config.modules.services.pi-web.ports.http}"
      "${primAddress}:${toString config.modules.services.pi-web.ports.http}"
    ];
    socketConfig = {
      FreeBind = true;
      NoDelay = true;
    };
  };
  systemd.services.pi-web-tailscale-proxy = {
    description = "PI WEB external address proxy";
    requires = [
      "pi-web.service"
      "pi-web-tailscale-proxy.socket"
    ];
    after = [
      "pi-web.service"
      "pi-web-tailscale-proxy.socket"
    ];
    serviceConfig = {
      ExecStart = "${pkgs.systemd}/lib/systemd/systemd-socket-proxyd 127.0.0.1:${toString config.modules.services.pi-web.ports.http}";
      DynamicUser = true;
      PrivateTmp = true;
      ProtectHome = true;
      ProtectSystem = "strict";
    };
  };

  # paseo tailscale proxy
  systemd.sockets.paseo-tailscale-proxy = {
    description = "Paseo proxy socket on external addresses";
    wantedBy = [ "sockets.target" ];
    listenStreams = [
      "${tailscaleIp4}:6767"
      "${primAddress}:6767"
    ];
    socketConfig = {
      FreeBind = true;
      NoDelay = true;
    };
  };
  systemd.services.paseo-tailscale-proxy = {
    description = "Paseo external address proxy";
    requires = [
      "paseo.service"
      "paseo-tailscale-proxy.socket"
    ];
    after = [
      "paseo.service"
      "paseo-tailscale-proxy.socket"
    ];
    serviceConfig = {
      ExecStart = "${pkgs.systemd}/lib/systemd/systemd-socket-proxyd 127.0.0.1:6767";
      DynamicUser = true;
      PrivateTmp = true;
      ProtectHome = true;
      ProtectSystem = "strict";
    };
  };

  services.paseo = {
    enable = true;
    user = "ramblurr";
    group = "ramblurr";
    dataDir = "/home/ramblurr/.local/state/paseo";
    inheritUserEnvironment = true;
    environment.PI_CODING_AGENT_DIR = "/home/ramblurr/.config/pi/agent";
  };
}
