{
  config,
  pkgs,
  ...
}: {
  virtualisation = {
    podman = {
      enable = true;
      defaultNetwork.settings.dns_enabled = true;
      extraPackages = [pkgs.zfs];
    };
  };
}
