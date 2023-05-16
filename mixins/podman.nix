{
  config,
  pkgs,
  ...
}: {
  environment.persistence."/persist" = {
    directories = [
      "/var/lib/containers"
    ];
  };
  virtualisation = {
    podman = {
      enable = true;
      defaultNetwork.settings.dns_enabled = true;
      extraPackages = [pkgs.zfs];
    };
  };
}
