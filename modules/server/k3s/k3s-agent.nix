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
  cfg = config.modules.agent.k3s-agent;
  k3s-package = pkgs.unstable.k3s_1_29;
in {
  options.modules.agent.k3s-agent = {
    enable = mkBoolOpt false;
    tokenFile = mkStrOpt "/etc/k3s-token";
    serverAddr = mkStrOpt "";
  };
  config = let
    extraFlags = [
      "--node-label \"k3s-upgrade=false\""
    ];
    combinedFlags = concatLists [extraFlags];
  in
    mkIf cfg.enable {
      modules.server.k3s-common.enable = true;
      services.k3s = {
        enable = true;
        package = k3s-package;
        role = "agent";
        serverAddr = cfg.serverAddr;
        tokenFile = cfg.tokenFile;
        extraFlags = concatStringsSep " " combinedFlags;
      };

      environment.systemPackages = [k3s-package];
    };
}
