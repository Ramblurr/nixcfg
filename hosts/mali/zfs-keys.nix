{
  config,
  lib,
  pkgs,
  ...
}:
{
  sops.secrets.tank2Key = {
    neededForUsers = true;
    mode = "400";
    owner = "root";
    group = "root";
  };
  sops.secrets.fastKey = {
    neededForUsers = true;
    mode = "400";
    owner = "root";
    group = "root";
  };

  # Gate import on successful key installation, without coupling imported pools
  # to the secret installer's lifetime during subsequent NixOS activations.
  systemd.services.zfs-import-tank2 = {
    wants = [ "sops-install-secrets-for-users.service" ];
    after = [ "sops-install-secrets-for-users.service" ];
    preStart = ''
      ${pkgs.systemd}/bin/systemctl is-active --quiet sops-install-secrets-for-users.service
      test -s ${lib.escapeShellArg config.sops.secrets.tank2Key.path}
    '';
  };
  systemd.services.zfs-import-fast = {
    wants = [ "sops-install-secrets-for-users.service" ];
    after = [ "sops-install-secrets-for-users.service" ];
    preStart = ''
      ${pkgs.systemd}/bin/systemctl is-active --quiet sops-install-secrets-for-users.service
      test -s ${lib.escapeShellArg config.sops.secrets.fastKey.path}
    '';
  };

  systemd.services.zfs-mount.requires = [ "zfs-import.target" ];

  environment.etc."mali-keys/tank2.key" = {
    user = "root";
    source = config.sops.secrets.tank2Key.path;
  };
  environment.etc."mali-keys/fast.key" = {
    user = "root";
    source = config.sops.secrets.fastKey.path;
  };
}
