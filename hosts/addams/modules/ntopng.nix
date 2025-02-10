{
  config,
  pkg,
  lib,
  ...
}:
{

  sops.secrets."maxmind/licenseKey" = { };
  services.geoipupdate = {
    enable = true;
    settings = {
      AccountID = config.repo.secrets.local.maxmind.accountId;
      EditionIDs = config.repo.secrets.local.maxmind.editionIds;
      LicenseKey = config.sops.secrets."maxmind/licenseKey".path;
    };
  };
  environment.persistence."/persist".directories = [ "/var/lib/ntopng" ];
  services.ntopng = {
    enable = true;
    interfaces = [
      config.repo.secrets.local.wan0.iface
      config.repo.secrets.local.untagged.iface
    ] ++ lib.mapAttrsToList (name: v: "me-${name}") config.repo.secrets.local.vlan;
  };
}
