{
  config,
  lib,
  ...
}:

let
  inherit (config.repo.secrets.global.domain) personal2 work;
  docsDomain = "docs.${work}";

  deployUsers = {
    ${personal2} = {
      uid = 993;
      gid = 991;
    };
    ${docsDomain} = {
      uid = 994;
      gid = 992;
      extraGroups = [ "nginx" ];
      authorizedKeys = true;
    };
  };

  mkDeployUser = username: attrs: {
    inherit username;
    uid = attrs.uid;
    gid = attrs.gid;
    homeDirectory = "/var/lib/${username}";
    extraGroups = attrs.extraGroups or [ ];
    homeDirectoryOnZfs.enable = true;
    homeDirectoryOnZfs.datasetName = "rpool/encrypted/safe/svc/${username}";
  };

  authorizedKeysText = builtins.concatStringsSep "\n" config.repo.secrets.global.pubKeys + "\n";
in
{
  modules.users.deploy-users = lib.mapAttrs mkDeployUser deployUsers;

  environment.etc = lib.mkMerge (
    lib.mapAttrsToList (
      username: attrs:
      lib.optionalAttrs (attrs.authorizedKeys or false) {
        "ssh/authorized_keys.d/${username}".text = authorizedKeysText;
      }
    ) deployUsers
  );
}
