{
  config,
  lib,
  ...
}:
let
  dataDir = "/var/lib/pocket-id";
  inherit (config.repo.secrets.local) domains;
in
{
  options.modules.services.pocket-id.enable = lib.mkEnableOption "Pocket ID";

  config = lib.mkIf config.modules.services.pocket-id.enable {
    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/pocket-id" = {
        mountpoint = dataDir;
        "com.sun:auto-snapshot" = "false";
      };
    };

    sops.templates.pocket-id-env = {
      owner = config.services.pocket-id.user;
      group = config.services.pocket-id.group;
      mode = "0400";
      restartUnits = [ "pocket-id.service" ];
      content = ''
        ENCRYPTION_KEY=${config.sops.placeholder.pocket-id-encryption-key}
      '';
    };

    systemd.services.pocket-id = {
      requires = [ "sops-install-secrets.service" ];
      after = [ "sops-install-secrets.service" ];
      unitConfig.RequiresMountsFor = [ dataDir ];
    };

    services.pocket-id = {
      enable = true;
      environmentFile = config.sops.templates.pocket-id-env.path;
      settings = {
        APP_URL = "https://id.${domains.home}";
        TRUST_PROXY = true;
        HOST = "127.0.0.1";
        PORT = 1411;
      };
    };
  };
}
