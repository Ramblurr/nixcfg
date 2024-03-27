{ options, config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.vpn.tailscale;
  username = config.modules.users.primaryUser.username;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.vpn.tailscale = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable {
    services.tailscale.enable = lib.mkIf cfg.enable true;

    boot.kernel.sysctl."net.ipv4.conf.all.forwarding" = lib.mkForce true;

    networking.firewall.trustedInterfaces = [ "tailscale0" ];
    systemd.services.tailscaled.serviceConfig.ExecStart = mkIf config.modules.vpn.mullvad.enable [
      ""
      "${pkgs.mullvad-vpn}/bin/mullvad-exclude ${pkgs.tailscale}/bin/tailscaled --state=/var/lib/tailscale/tailscaled.state --socket=/run/tailscale/tailscaled.sock --port=\${PORT} $FLAGS"
    ];

    environment.persistence = mkIf config.modules.impermanence.enable {
      "/persist".directories = [ "/var/lib/tailscale" "/var/cache/tailscale" ];
    };
  };
}
