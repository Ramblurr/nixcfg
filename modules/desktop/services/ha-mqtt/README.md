# Home Assistant MQTT desktop actions

`ha-mqtt` is an outbound-only systemd user service. It leaves hacompanion's
registration, sensors, notifications, and credentials alone. No desktop HTTP
listener or new firewall port is needed.

## Configuration

```nix
modules.desktop.services.ha-mqtt = {
  enable = true;
  topicPrefix = "ha-mqtt/${config.networking.hostName}"; # also the default
  mqtt = {
    host = "<confirmed MQTT broker hostname>";
    port = 8883;
    tls.enable = true;
    username = config.networking.hostName;
    passwordFile = config.sops.secrets.mqtt-password.path;
  };
  shutdown.enable = false;
};
sops.secrets.mqtt-password = {
  owner = config.modules.users.primaryUser.username;
  mode = "0400";
};
```

This is an example, not a statement that the production broker has a TLS
listener. Confirm host, port, TLS listener, and ACL enforcement before enabling.
The port defaults to 8883 with TLS, otherwise 1883. TLS uses system trust;
`mqtt.tls.caFile` optionally supplies a CA certificate. Verification stays on.
Topic prefixes must be nonblank, without `+`, `#`, NUL, or a trailing slash.

The password file contains only the plaintext password, not JSON or environment
assignments. One terminal LF/CRLF is removed; other whitespace is preserved.
The file must resolve outside `/nix/store`, be owned by the desktop user, and
have mode 0400 or 0600. Missing or unsafe credentials fail closed; systemd retries.
MQTTX receives a generated mode-0600 config inside a mode-0700 temporary directory
under the service's private runtime directory. Passwords never enter argv,
store-bound Nix configuration, or logs. MQTTX diagnostics are discarded; the
bridge logs fixed outcome names rather than exceptions or received payloads.
Do not enable MQTTX debug logging when investigating credentials.

Use a separate broker account for each host. Without TLS, credentials cross the
network without transport encryption.

System-level secret delivery must precede the desktop user manager. For SOPS
systemd activation, order `sops-install-secrets.service` before that manager.
For user-manager credential delivery, set `credentialUnits` to the delivering
units; they become `Requires` and `After` dependencies. Do not put system-manager
unit names in that option. A password rotation takes effect on bridge restart.

The service is restricted with `ConditionUser` to the configured primary user,
and starts/stops with `graphical-session.target`. It has explicit runtime tools
and session-bus/PipeWire access via `%t`. It is not a root service and does not
promise operation while logged out, even with lingering enabled.

## MQTT contract and ACLs

For prefix `P`:

| Topic | Purpose | Payload | QoS | Retain |
| --- | --- | --- | --- | --- |
| `P/command` | Commands | One exact action below | 0 | false |
| `P/speaker/muted` | Observed speaker mute state | `1` muted, `0` unmuted | 1 | true |

The desktop client needs subscribe permission only on its exact command topic
and publish permission only on its exact state topic. For Mosquitto, a host ACL
can use `topic read ha-mqtt/desktop/command` and
`topic write ha-mqtt/desktop/speaker/muted` under `user desktop`. HA needs the converse
permissions. Enforce these on the broker; account creation alone is not proof of
ACL isolation. This implementation does not modify the production broker.

The runtime requires MQTT 5. MQTTX `sub --output-mode clean` emits successive
complete JSON objects including the packet's retain flag and raw payload bytes.
The bridge parses JSON values, never console lines, validates the exact topic,
and compares the accepted ASCII action with the original bytes. It rejects
retained/duplicate packets and unknown, multiline, argument-bearing or malformed
payloads. The advertised maximum packet size is 1024 bytes; valid commands are
at most 16 bytes.

Subscriptions use clean sessions, expiry zero, QoS 0, retain handling 2, and retain
as published true. Retained snapshots are not sent at subscription, and live
retained publications are rejected too. MQTTX reconnect is disabled: the bridge
supervises one subscription and waits three seconds before creating a fresh
connection. Subscriber and publisher IDs are distinct UUID-based IDs. Desktop
actions and state publishers have five-second deadlines; systemd kills the
whole control group on stop. There is no command queue across downtime.

State uses QoS 1 so successful completion includes a broker acknowledgement.
A small local patch to nixpkgs MQTTX 1.12.1 makes one-shot publication fail with a
nonzero exit when its connection closes before publication succeeds. Unpatched
1.12.1 returns zero even on connection refusal. The bridge does not parse its
human-readable publication logs. Recheck this patch when updating MQTTX.

State is **last observed**, not continuously synchronized. Keyboard/desktop
changes are not watched. Initial/reconnect publication is not implemented;
publish `speaker-get-mute` before relying on freshness. Failed actions, failed
queries, unparseable state, and failed publication leave the prior retained state
untouched and log `speaker-action-or-publish-failed`.

The four speaker CLI commands remain installed by the PipeWire module. Shared
helpers use `amixer -D pipewire` and the Master control. Failed queries no longer
masquerade as unmuted or drive a toggle decision.

## Home Assistant scripts

Each entry below can be used as a script invoked by a dashboard button. Replace
the example prefix `ha-mqtt/desktop` with the configured topic prefix.

```yaml
speaker_mute:
  sequence:
    - action: mqtt.publish
      data:
        topic: ha-mqtt/desktop/command
        payload: speaker-mute
        qos: 0
        retain: false
speaker_unmute:
  sequence:
    - action: mqtt.publish
      data:
        topic: ha-mqtt/desktop/command
        payload: speaker-unmute
        qos: 0
        retain: false
speaker_toggle:
  sequence:
    - action: mqtt.publish
      data:
        topic: ha-mqtt/desktop/command
        payload: speaker-toggle
        qos: 0
        retain: false
speaker_get_mute:
  sequence:
    - action: mqtt.publish
      data:
        topic: ha-mqtt/desktop/command
        payload: speaker-get-mute
        qos: 0
        retain: false
desktop_shutdown:
  sequence:
    - action: mqtt.publish
      data:
        topic: ha-mqtt/desktop/command
        payload: shutdown
        qos: 0
        retain: false
```

## Shutdown safety

Shutdown is separately disabled by default. `shutdown.gracePeriodMs` defaults to
60000 (allowed range 1000–3600000). `shutdown.dryRun` defaults to true even when
shutdown is enabled. Leave it true for validation.

A critical notification offers **Cancel shutdown** and explains that dismissing
it also cancels. Mako can invoke the named action through `makoctl invoke cancel`;
its ordinary dismissal also cancels. Only dunstify close reason `1` (expiry),
after at least the configured monotonic grace period, permits a poweroff request.
`cancel`, reason `2` (dismissed), and reason `3` (closed) cancel. Early expiry,
missing action capability, notification failure, unexpected results, or no
response by grace plus five seconds fail closed. Service stop kills the pending
interaction. Additional shutdown commands do not restart or duplicate a pending
countdown; speaker actions remain available.

Logs distinguish `shutdown-pending`, `shutdown-cancelled`, `shutdown-failed`,
`shutdown-dry-run`, and `shutdown-poweroff-requested`; disabled and duplicate
requests have separate outcomes. Poweroff uses the user's existing logind/polkit
permissions. A failure is reported, not worked around with sudo or new grants.

Mako with dunstify 1.13.2 was tested on a separate D-Bus session: cancel returned
`cancel`, dismissal `2`, expiry `1` after the deadline, and daemon termination
produced no expiry (the watchdog must fail closed). Do not enable real shutdown
with rules that hide these notifications or remove their cancellation affordance.
Revalidate after changing the notification daemon or its policy.

## Validation and deployment

`checks.<system>.ha-mqtt` evaluates module defaults, overrides, user scoping,
dependencies, and legacy retirement, then runs the disposable-Mosquitto tests.
Tests use the real bridge, real patched MQTTX, and real shared speaker helpers;
only amixer, dunstify, and systemctl are harmless process-boundary substitutes.
No desktop bus or runtime directory is passed to them.

The optional `python3 tests/ha-mqtt-notifications.py` probe requires real `mako`,
`makoctl`, `dunstify`, and `dbus-run-session` on PATH and a Wayland display. It
shows labelled harmless notifications on a private bus; it never calls poweroff
or changes the normal notification daemon.

Before deployment: confirm the broker listener and host ACLs, review the runtime
secret permissions, use dry-run shutdown, and obtain operational approval. Commit
public changes, refresh only the private wrapper's `nixcfg` input, re-enter its
devshell, and use the repository's `build <host>` / approved `deploy` wrappers.
Never use actual poweroff as an incidental validation step.

The legacy HTTP service, Python helper, token-delivery declaration, and port-5001
firewall contribution are retired. The operator should remove any obsolete
`HA_SHUTDOWN_TOKEN` value from encrypted secrets separately; encrypted values
were not edited here.
