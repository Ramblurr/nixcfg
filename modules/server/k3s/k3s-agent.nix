{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.server.k3s-agent;
in {
  options.modules.server.k3s-agent = {
    enable = mkBoolOpt false;
    started = mkBoolOpt true;
    tokenFile = mkStrOpt "/etc/k3s-token";
    serverAddr = mkStrOpt "";
    nodeIp = mkStrOpt "";
  };
  config = let
    extraFlags = [
      "--node-label \"k3s-upgrade=false\""
      "--pause-image registry.k8s.io/pause:3.9"
      "--node-ip ${cfg.nodeIp}"
      "--kubelet-arg image-gc-low-threshold=50"
      "--kubelet-arg image-gc-high-threshold=55"
    ];
    combinedFlags = concatLists [extraFlags];
  in
    mkIf cfg.enable {
      modules.server.k3s-common.enable = true;
      services.k3s = {
        enable = cfg.started;
        role = "agent";
        serverAddr = cfg.serverAddr;
        tokenFile = cfg.tokenFile;
        extraFlags = concatStringsSep " " combinedFlags;
      };
    };
}
