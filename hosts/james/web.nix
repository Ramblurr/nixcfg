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

  modules.services.staticSites.sites.epsilon = {
    domain = "stardustdb.guide";
    path = "/dust/";
    directory = "/var/lib/static-web/stardustdb.guide/epsilon";
    authorizedKeys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIF6c85Vp8lcCzW02rxJX8qvZDA56Sx00sCUMZZHpzNns"
    ];
  };

  systemd.tmpfiles.rules = [
    "d /var/lib/static-web/stardustdb.guide 0755 ${caddyUser} ${caddyGroup} - -"
    "d '/var/lib/static-web' 0751 ${caddyUser} ${caddyGroup} - -"
    # rsync 3.5 opens each destination ancestor for reading, not just traversal.
    "a+ /var/lib/static-web - - - - u:epsilon-deploy:r-x"
    "Z '${partnerPath}' - ${caddyUser} ${caddyGroup} - -"
  ];
}
