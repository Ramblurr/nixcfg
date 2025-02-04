{ lib, config, ... }:
let
  dhcpLib = import ./helpers.nix { inherit lib; };
  keaddnsUser = "kea";

  pdnsServer = [
    {
      ip-address = "127.0.0.1";
      port = 8853;
    }
  ];

  vlans = [
    "local"
    "guest"
    "prim"
    "mgmt"
    "data"
    "iot"
    "not"
  ];
in
{
  # add user, needed to access the secret
  users = {
    users.${keaddnsUser} = {
      isSystemUser = true;
      group = keaddnsUser;
    };
    groups.${keaddnsUser} = { };
  };

  sops.secrets."kea/tsig-key" = {
    owner = keaddnsUser;
    group = keaddnsUser;
  };

  services.kea = {
    dhcp4.settings = {
      dhcp-ddns.enable-updates = true;
      ddns-replace-client-name = "when-not-present";
      ddns-update-on-renew = true; # always update when a lease is renewed, in case I lost the DNS server database
      ddns-override-client-update = true; # always generate ddns update request ignoring the client's wishes not to
      ddns-override-no-update = true; # same as above but for different client's wishes
    };
    dhcp-ddns = {
      enable = true;
      settings = {
        tsig-keys = [
          {
            name = "kea";
            algorithm = "hmac-sha512";
            secret-file = "${config.sops.secrets."kea/tsig-key".path}";
          }
        ];
        forward-ddns = {
          ddns-domains = [
            {
              name = "${config.repo.secrets.global.domain.home}.";
              key-name = "kea";
              dns-servers = pdnsServer;
            }
          ];
        };
        reverse-ddns = {
          ddns-domains = map (zone: {
            key-name = "kea";
            name = zone;
            dns-servers = pdnsServer;
          }) config.repo.secrets.local.reverseZones;
        };
      };
    };
  };
}
