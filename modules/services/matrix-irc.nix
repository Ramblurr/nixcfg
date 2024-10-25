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

    systemd.services.heisenbridge = {
      after = [
        "network.target"
        "matrix-synapse.service"
      ];
      bindsTo = [ "matrix-synapse.service" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Restart = "on-failure";
      };
      unitConfig = {
        StartLimitBurst = 3;
        StartLimitIntervalSec = "30s";
      };
    };
    # TODO: Make work in cases where this isn't on the same machine.
    services.matrix-synapse.settings.app_service_config_files = [
      "/var/lib/heisenbridge/registration.yml"
    ];
  };
}
