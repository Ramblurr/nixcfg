{
  pkgs,
  config,
  ...
}: {
  config = {
    services.tailscale.enable = true;

    boot.kernel.sysctl."net.ipv4.conf.all.forwarding" = true;

    networking.firewall.trustedInterfaces = ["tailscale0"];
  };
}
