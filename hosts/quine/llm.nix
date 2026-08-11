{
  pkgs,
  config,
  lib,
  inputs,
  ...
}:
let
  tailscaleIp4 = "100.93.18.79";
  primAddress = "10.9.4.3";
  homeDomain = config.repo.secrets.global.domain.home;
in
{
  environment.systemPackages = [ inputs.smolvm.packages.${pkgs.stdenv.hostPlatform.system}.default ];
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

  systemd.services.paseo.serviceConfig.ExecStart =
    lib.mkForce "${lib.getExe pkgs.zsh} -c 'exec ${config.services.paseo.package}/bin/paseo-server${
      lib.optionalString (!config.services.paseo.relay.enable) " --no-relay"
    }'";

  services.paseo = {
    enable = true;
    user = "ramblurr";
    group = "ramblurr";
    dataDir = "/home/ramblurr/.local/state/paseo";
    inheritUserEnvironment = true;
    hostnames = [
      ".${homeDomain}"
      "localhost"
    ];
    listenAddress = "127.0.0.1";
    port = 6767;
    environment = {
      PI_CODING_AGENT_DIR = "/home/ramblurr/.config/pi/agent";
      PASEO_RELAY_ENABLED = "false";
      PASEO_WEB_UI_ENABLED = "true";
      PASEO_TRUSTED_PROXIES = "10.9.4.17/32";
    };
  };

  networking.firewall.interfaces."tailscale0".allowedTCPPorts = [ config.services.paseo.port ];
  networking.firewall.interfaces."prim".allowedTCPPorts = [ config.services.paseo.port ];
}
