{
  pkgs,
  config,
  lib,
  ...
}: {
  services.tailscale.enable = lib.mkIf (config.deviceSpecific.vpn.tailscale.enable) true;

  boot.kernel.sysctl."net.ipv4.conf.all.forwarding" = true;

  networking.firewall.trustedInterfaces = ["tailscale0"];
  systemd.services.tailscaled.serviceConfig.ExecStart = [
    ""
    "${pkgs.mullvad-vpn}/bin/mullvad-exclude ${pkgs.tailscale}/bin/tailscaled --state=/var/lib/tailscale/tailscaled.state --socket=/run/tailscale/tailscaled.sock --port=\${PORT} $FLAGS"
  ];

  environment.persistence = {
    "/persist".directories = [
      "/var/lib/tailscale"
      "/var/cache/tailscale"
    ];
  };
}
