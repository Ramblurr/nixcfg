/*
  From @oddlama
  ref: https://github.com/oddlama/nix-config/blob/a3f74254e73b4698b3d114a7dbe1ae2ef787c2b9/modules/distributed-config.nix
*/
{
  config,
  lib,
  nodes,
  ...
}:
let
  inherit (lib)
    attrNames
    concatMap
    concatStringsSep
    foldl'
    getAttrFromPath
    mkMerge
    mkOption
    mkOptionType
    optionals
    recursiveUpdate
    setAttrByPath
    types
    ;

  nodeName = config.node.name;
  mkForwardedOption =
    path:
    mkOption {
      type = mkOptionType {
        name = "Same type that the receiving option `${concatStringsSep "." path}` normally accepts.";
        merge =
          _loc: defs:
          builtins.filter (x: builtins.isAttrs x -> ((x._type or "") != "__distributed_config_empty")) (
            map (x: x.value) defs
          );
      };
      default = {
        _type = "__distributed_config_empty";
      };
      description = ''
        Anything specified here will be forwarded to `${concatStringsSep "." path}`
        on the given node. Forwarding happens as-is to the raw values,
        so validity can only be checked on the receiving node.
      '';
    };

  forwardedOptions = [
    [
      "sops"
      "secrets"
    ]
    #[
    #  "networking"
    #  "nftables"
    #  "chains"
    #]
    [
      "services"
      "nginx"
      "upstreams"
    ]
    [
      "services"
      "nginx"
      "virtualHosts"
    ]
  ];

  attrsForEachOption =
    f: foldl' (acc: path: recursiveUpdate acc (setAttrByPath path (f path))) { } forwardedOptions;
in
{
  options.nodes = mkOption {
    description = "Options forwarded to the given node.";
    default = { };
    type = types.attrsOf (
      types.submodule {
        options = attrsForEachOption mkForwardedOption;
      }
    );
  };

  config =
    let
      getConfig =
        path: otherNode:
        let
          otherNode2 = lib.traceVal otherNode;
          cfg = nodes.${otherNode2}.config.nodes.${nodeName} or null;
        in
        optionals (cfg != null) (getAttrFromPath path cfg);
      mergeConfigFromOthers = path: mkMerge (concatMap (getConfig path) (attrNames nodes));
    in
    attrsForEachOption mergeConfigFromOthers;
}
