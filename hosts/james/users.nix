{
  config,
  lib,
  ...
}:

let
  inherit (config.repo.secrets.global.domain) work;
  caseyLink = config.repo.secrets.global.domain."casey.link";
  docsDomain = "docs.${work}";
  authorizedKeys = config.repo.secrets.global.pubKeys;

  deployUsers = {
    ${caseyLink} = {
      inherit authorizedKeys;
      uid = 993;
      gid = 991;
      homeManager.enable = true;
    };
    ${docsDomain} = {
      inherit authorizedKeys;
      uid = 1994;
      gid = 1992;
      extraGroups = [ "caddy" ];
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
