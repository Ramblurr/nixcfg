{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let

  inherit (config.networking) hostName;
  nets = config.site.net;
  hostConfig = config.site.hosts.${hostName};
  hostBridges = builtins.attrNames (
    lib.filterAttrs (_: { type, ... }: type == "bridge") hostConfig.interfaces
  );
in
{
  imports = [
    ../../config/site.nix
    ../../modules/site
    ../../modules/site-net
  ];

  home-ops.microvm-host = {
    enable = true;
    baseZfsDataset = "rpool/encrypted/safe/microvms";
  };
  users.users.root.openssh.authorizedKeys.keys = config.repo.secrets.global.pubKeys;

}
