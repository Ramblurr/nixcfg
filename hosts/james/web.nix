{ config, pkgs, ... }:

let
  caddyUser = config.services.caddy.user;
  caddyGroup = config.services.caddy.group;
  partnerPath = "/var/lib/static-web/${config.repo.secrets.global.domain.partner}";
in
{
  modules.zfs.datasets.properties = {
    "rpool/encrypted/safe/svc/static-web"."mountpoint" = "/var/lib/static-web";
  };

  users.groups.epsilon-deploy = { };
  users.users.epsilon-deploy = {
    isSystemUser = true;
    group = "epsilon-deploy";
    home = "/var/lib/static-web/stardustdb.guide/epsilon";
    shell = pkgs.bash;
    openssh.authorizedKeys.keys = [
      "restrict ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIF6c85Vp8lcCzW02rxJX8qvZDA56Sx00sCUMZZHpzNns"
    ];
  };
  environment.systemPackages = [ pkgs.rsync ];

  systemd.tmpfiles.rules = [
    "d /var/lib/static-web/stardustdb.guide 0755 ${caddyUser} ${caddyGroup} - -"
    "d /var/lib/static-web/stardustdb.guide/epsilon 0755 epsilon-deploy epsilon-deploy - -"
    "d '/var/lib/static-web' 0751 ${caddyUser} ${caddyGroup} - -"
    "Z '${partnerPath}' - ${caddyUser} ${caddyGroup} - -"
  ];
}
