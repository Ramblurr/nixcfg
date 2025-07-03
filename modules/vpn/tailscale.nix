{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
let
  cfg = config.modules.vpn.tailscale;
  username = config.modules.users.primaryUser.username;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.vpn.tailscale = {
    enable = lib.mkEnableOption "";
    useRoutingFeatures = mkOption {
      type = types.enum [
        "none"
        "client"
        "server"
        "both"
      ];
      default = "none";
    };
    exitNode = {
      enable = lib.mkEnableOption "";
      networkDev = lib.mkOption { type = lib.types.string; };
    };
  };
  config = mkIf cfg.enable {
    services.tailscale.enable = lib.mkIf cfg.enable true;
    services.tailscale.useRoutingFeatures = cfg.useRoutingFeatures;
    # ref: https://github.com/tailscale/tailscale/issues/3310
    networking.firewall.checkReversePath = "loose";

    boot.kernel.sysctl = {
      "net.ipv4.conf.all.forwarding" = lib.mkForce true;
      "net.ipv6.conf.all.forwarding" = true;
    };
    services.networkd-dispatcher = lib.mkIf cfg.exitNode.enable {
      enable = true;
      rules."50-tailscale" = {
        onState = [ "routable" ];
        script = ''
          ${lib.getExe pkgs.ethtool} -K ${cfg.exitNode.networkDev} rx-udp-gro-forwarding on rx-gro-list off
        '';
      };
    };

    networking.firewall.trustedInterfaces = [ "tailscale0" ];
    systemd.services.tailscaled.serviceConfig.ExecStart = mkIf config.modules.vpn.mullvad.enable [
      ""
      "${pkgs.mullvad-vpn}/bin/mullvad-exclude ${pkgs.tailscale}/bin/tailscaled --state=/var/lib/tailscale/tailscaled.state --socket=/run/tailscale/tailscaled.sock --port=\${PORT} $FLAGS"
    ];

    environment.persistence = mkIf config.modules.impermanence.enable {
      "/persist".directories = [
        "/var/lib/tailscale"
        "/var/cache/tailscale"
      ];
    };
  };
}
