# As of Aug 2023 Hetzner Cloud CCX* vms support UEFI by default
{
  config,
  modulesPath,
  lib,
  pkgs,
  ...
}:

{
  imports = [
    "${modulesPath}/profiles/qemu-guest.nix"
  ];

  boot.loader.grub.enable = lib.mkForce false;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  networking.useNetworkd = true;
  networking.useDHCP = true;
  networking.dhcpcd.enable = false;
  systemd.network = {
    enable = true;
    wait-online.ignoredInterfaces = [
      "lo"
      "tailscale0"
    ];
  };

  services.timesyncd.enable = true;
  services.timesyncd.servers = [
    "ntp1.hetzner.de"
    "ntp2.hetzner.com"
    "ntp3.hetzner.net"
  ];
  services.resolved.enable = true;

  # Needed by the Hetzner Cloud password reset feature.
  services.qemuGuest.enable = lib.mkDefault true;
  # https://discourse.nixos.org/t/qemu-guest-agent-on-hetzner-cloud-doesnt-work/8864/2
  systemd.services.qemu-guest-agent.path = [ pkgs.shadow ];
}
