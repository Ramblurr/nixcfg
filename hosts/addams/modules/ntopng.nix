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
      "wan0"
      "lan0"
      "iot"
      "inot"
      "mgmt"
      "vpn"
      "prim"
      "guest"
      "svc"
      "data"
    ];
  };
}
