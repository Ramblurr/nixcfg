{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.services.matrix-synapse.bridges.irc;
  rootCfg = config.modules.services.matrix-synapse;
  service = "matrix-synapse";
  dataDir = "${rootCfg.dataDir}/mautrix-irc";
in
{
  options.modules.services.matrix-synapse.bridges.irc = {
    enable = lib.mkEnableOption "heisenbridge for personal irc matrix bridging";
  };
  config = lib.mkIf cfg.enable {
    services.heisenbridge = {
      enable = true;
      homeserver = "https://${config.modules.services.matrix-synapse.domain}";
    };
  };
}
