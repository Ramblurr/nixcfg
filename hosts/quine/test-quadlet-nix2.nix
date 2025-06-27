{
  config,
  lib,
  pkgs,
  ...
}:

{

  virtualisation.quadlet.enable = true;
  users.users.quadlet = {
    isSystemUser = true;
    uid = 444;
    linger = true;
    home = "/var/lib/quadlet";
    createHome = true;
    shell = pkgs.shadow;
    autoSubUidGidRange = true;
    group = "quadlet";
  };
  users.groups.quadlet = {
    gid = 444;
  };
  virtualisation.quadlet = {
    containers = {
      nginx = {
        containerConfig = {
          Image = "docker-archive:${pkgs.dockerTools.examples.nginx}";
          PublishPort = [ "8080:80" ];
          Environment = {
            TZ = "Europe/Berlin";
          };
        };
      };
    };
  };
}
