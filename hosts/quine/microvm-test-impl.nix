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

  genGuestSecret = hostname: {
    "microvm-${hostname}-sops-key" = {
      sopsFile = ../../guests/${hostname}/secrets.sops.yaml;
      key = "ssh_host_ed25519_key";
      owner = "microvm";
      mode = "400";
    };
  };
  guests = [
    "claude-test"
  ];
in
{
  imports = [
    ../../modules/site
    ../../modules/site-net
  ];

  home-ops.microvm-host = {
    enable = true;
    baseZfsDataset = "rpool/encrypted/safe/microvms";
  };

  sops.secrets = lib.mori.reduceAttrs genGuestSecret guests;
}
