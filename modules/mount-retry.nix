{
  config,
  lib,
  utils,
  ...
}:
let
  mountPoints = config.modules.mount-retry.mountPoints;
  unitName = path: "${utils.escapeSystemdPath path}.mount";
in
{
  options.modules.mount-retry.mountPoints = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [ ];
    description = "nofail mounts whose mount and unmount give up after 30 s and whose failed start is retried every 30 s until it succeeds, for servers that may be away while this host boots.";
  };
  config = lib.mkIf (mountPoints != [ ]) {
    assertions = map (path: {
      assertion = lib.elem "nofail" (config.fileSystems.${path}.options or [ ]);
      message = "modules.mount-retry: ${path} must be a fileSystems entry with nofail, so boot does not wait for it.";
    }) mountPoints;
    systemd.units = lib.listToAttrs (
      map (
        path:
        lib.nameValuePair (unitName path) {
          overrideStrategy = "asDropin";
          text = ''
            [Unit]
            OnFailure=mount-retry@${unitName path}.service
            [Mount]
            TimeoutSec=30s
          '';
        }
      ) mountPoints
    );
    systemd.services."mount-retry@" = {
      description = "Retry %i until it mounts";
      unitConfig.StartLimitIntervalSec = 0;
      serviceConfig = {
        ExecStart = "${config.systemd.package}/bin/systemctl start %i";
        Restart = "on-failure";
        RestartSec = "30s";
      };
    };
  };
}
