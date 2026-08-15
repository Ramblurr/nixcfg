{ config, ... }:

let
  caddyUser = config.services.caddy.user;
  caddyGroup = config.services.caddy.group;
  partnerPath = "/var/lib/static-web/${config.repo.secrets.global.domain.partner}";
in
{
  modules.zfs.datasets.properties = {
    "rpool/encrypted/safe/svc/static-web"."mountpoint" = "/var/lib/static-web";
  };

  systemd.tmpfiles.rules = [
    "d '/var/lib/static-web' 0751 ${caddyUser} ${caddyGroup} - -"
    "Z '${partnerPath}' - ${caddyUser} ${caddyGroup} - -"
  ];
}
