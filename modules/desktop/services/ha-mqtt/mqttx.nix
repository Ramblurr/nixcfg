{ pkgs }:
# MQTTX 1.12.1 exits zero on a failed connection before publishing. Do not infer
# delivery from its human-oriented logs; require successful QoS 1 completion.
assert pkgs.mqttx-cli.version == "1.12.1";
pkgs.mqttx-cli.overrideAttrs (old: {
  patches = (old.patches or [ ]) ++ [ ./mqttx-publish-exit.patch ];
})
