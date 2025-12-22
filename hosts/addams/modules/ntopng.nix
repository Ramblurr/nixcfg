{
  config,
  ...
}:
{

  sops.secrets."maxmind/licenseKey" = { };
  services.geoipupdate = {
    enable = true;
    settings = {
      AccountID = config.repo.secrets.local.maxmind.accountId;
      EditionIDs = [
        "GeoLite2-ASN"
        "GeoLite2-City"
        "GeoLite2-Country"
      ];
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
  systemd.services.ntopng.serviceConfig = {
    Restart = "always";
    RestartSec = "20s";
  };
}
