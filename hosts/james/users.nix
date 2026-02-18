{
  config,
  lib,
  ...
}:

let
  inherit (config.repo.secrets.global.domain) personal2 work;
  docsDomain = "docs.${work}";
  authorizedKeys = config.repo.secrets.global.pubKeys;

  deployUsers = {
    ${personal2} = {
      inherit authorizedKeys;
      uid = 993;
      gid = 991;
      homeManager.enable = true;
    };
    ${docsDomain} = {
      inherit authorizedKeys;
      uid = 1994;
      gid = 1992;
      extraGroups = [ "nginx" ];
      homeManager.enable = false;
    };
  };

  mkDeployUser = username: attrs: {
    inherit username;
    inherit (attrs) uid;
    inherit (attrs) gid;
    homeDirectory = "/var/lib/${username}";
    extraGroups = attrs.extraGroups or [ ];
    authorizedKeys = attrs.authorizedKeys or [ ];
    homeDirectoryOnZfs.enable = true;
    homeDirectoryOnZfs.datasetName = "rpool/encrypted/safe/svc/${username}";
    homeManager.enable = attrs.homeManager.enable;
  };
in
{
  modules.users.deploy-users = lib.mapAttrs mkDeployUser deployUsers;
}
